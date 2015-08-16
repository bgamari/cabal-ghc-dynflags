{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | One of the challenges of using the GHC API for external tooling
-- is handling integration with Cabal. This library provides a simple
-- interface for configuring GHC's 'DynFlags' as Cabal would have,
-- allowing seamless tooling use on Cabal projects.
--
-- A typical usage might look like,
--
-- > import GHC
-- > import qualified GHC.Paths
-- > import qualified Distribution.Verbosity as Verbosity
-- > import GHC.Cabal
-- >
-- > main = runGhc (Just GHC.Paths.libdir) $ do
-- >     dflags <- GHC.getSessionDynFlags
-- >     -- Use default DynFlags if we aren't in a Cabal project
-- >     (dflags', _) <- fromMaybe (dflags, Nothing)
-- >                     <$> liftIO (initCabalDynFlags defaultConfig dflags)
-- >     GHC.setSessionDynFlags dflags'
-- >
-- >     -- Standard GHC API usage goes here
--
-- In addition to the 'DynFlags', 'initCabalDynFlags' also offers a variety of
-- information about the current project in the form of 'CabalDetails'. Perhaps
-- the most useful information offered is 'cdComponent', which can be used in
-- conjunction with 'componentTargets' to generate the GHC 'Target's
-- defined by the component.
--
-- For instance, to automatically bring the project's modules into scope,
--
-- > (dflags', cd) <- maybe (dflags, Nothing) (\(a,b)->(a, Just b))
-- >                  <$> liftIO (initCabalDynFlags defaultConfig dflags)
-- > GHC.setSessionDynFlags dflags'
-- > traverse (GHC.setTargets . componentTargets . cdComponent) cd
--
-- * Configuration
--
-- Cabal has many knobs. This package exposes a few of the more commonly-needed
-- ones in 'Config', although in many cases you won't need them.
--

module GHC.Cabal (
      -- * Initializing GHC DynFlags for Cabal packages
      initCabalDynFlags
    , CabalDetails(..)
      -- * Configuration
    , Config(..)
      -- * Discovering a project's modules
    , componentTargets
    ) where

import Control.Monad (guard, msum, mzero)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class
import Data.Maybe (listToMaybe, fromMaybe)
import System.Environment (lookupEnv)

import Control.Applicative ((<|>))
import Distribution.Verbosity
import Distribution.Simple.Utils (defaultPackageDesc, warn, debug, findPackageDesc)
import Distribution.Simple.Program (defaultProgramConfiguration)
import qualified Distribution.Simple.BuildTarget as BuildTarget
import qualified Distribution.Simple.Setup as Setup
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse as PD
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Compiler as Compiler
import qualified Distribution.Simple.GHC      as CGHC
import qualified Distribution.Simple.Program.GHC as CGHC
import qualified Distribution.ModuleName
import Distribution.Text (display)
import qualified GHC
import DynFlags (DynFlags, parseDynamicFlagsCmdLine)
import qualified SrcLoc

#if ! MIN_VERSION_Cabal(1,20,0)
import System.IO.Error (catchIOError)
#endif

-- | Useful information about the current Cabal package
data CabalDetails = CabalDetails
    { -- | Cabal's 'PD.PackageDescription'
      cdPackageDescription      :: PD.PackageDescription
      -- | The 'LocalBuildInfo' being use to configure GHC
    , cdLocalBuildInfo          :: LocalBuildInfo
      -- | The name of the component being used
    , cdComponent               :: Component
      -- | The 'ComponentLocalBuildInfo' of the Cabal 'Component' being use to configure GHC
    , cdComponentLocalBuildInfo :: ComponentLocalBuildInfo
    }

-- | Various configuration options that one may want to override.
-- If not just use 'defaultConfig'.
data Config = Config { -- | How verbose should we be?
                       verbosity  :: Verbosity
                       -- | @dist/@ prefix ('Nothing' for default)
                     , distDir    :: Maybe FilePath
                     }

-- | A sane default configuration.
defaultConfig :: Config
defaultConfig = Config { verbosity = normal
                       , distDir = Nothing
                       }

-- | Find the @dist/@ directory.
getDistDir :: Config -> IO FilePath
getDistDir (Config {..}) = do
#if MIN_VERSION_Cabal(1,23,0)
    findDistPrefOrDefault (maybe Noflag Flag $ distDir)
#else
    return $ fromMaybe Setup.defaultDistPref distDir
#endif

-- | Find the Cabal package description in the current directory (if any)
findPackageDescription :: Config -> MaybeT IO FilePath
findPackageDescription (Config {..}) = do
#if MIN_VERSION_Cabal(1,20,0)
    let warnNoCabal _err = lift (warn verbosity "Couldn't find cabal file") >> mzero
    either warnNoCabal return =<< lift (findPackageDesc ".")
#else
    MaybeT $ catchIOError (Just <$> findPackageDesc ".") (pure Nothing)
#endif

-- | Modify a set of 'DynFlags' to match what Cabal would produce.
initCabalDynFlags :: Config
                  -> DynFlags
                  -> IO (Maybe (DynFlags, CabalDetails))
initCabalDynFlags cfg@(Config {..}) dflags0 = runMaybeT $ do
    pdfile <- findPackageDescription cfg
    gpkg_descr <- lift $ PD.readPackageDescription verbosity pdfile
    dist <- lift $ getDistDir cfg
    lbi <- lift $ Configure.getPersistBuildConfig dist

    let programsConfig = defaultProgramConfiguration
    (comp, compPlatform, programsConfig') <- lift $
        Configure.configCompilerEx (Just Compiler.GHC) Nothing Nothing
                                   (withPrograms lbi) (lessVerbose verbosity)

    -- TODO: is any of this correct?
    let pkg_descr = case finalizePackageDescription
                             []
                             (const True)
                             compPlatform
#if MIN_VERSION_Cabal(1,22,0)
                             (Compiler.compilerInfo comp)
#else
                             (Compiler.compilerId comp)
#endif
                             []
                             gpkg_descr
                             of
                        Right (pd,_) -> pd
                        -- This shouldn't happen since we claim dependencies can always be satisfied
                        Left err     -> error "missing dependencies"

    let warnNoComps = do
            lift $ warn verbosity $ "Found no buildable components in "++pdfile
            mzero
    comp <- maybe warnNoComps return $ listToMaybe $ LBI.pkgEnabledComponents pkg_descr
    let bi = LBI.componentBuildInfo comp
        compName = LBI.componentName comp
        clbi = getComponentLocalBuildInfo lbi compName

    dflags <- lift $ initBuildInfoDynFlags verbosity lbi bi clbi dflags0
    let cd = CabalDetails { cdPackageDescription      = pkg_descr
                          , cdLocalBuildInfo          = lbi
                          , cdComponent               = comp
                          , cdComponentLocalBuildInfo = clbi
                          }
    return (dflags, cd)

-- | The GHC targets associated with a Cabal 'Component'.
componentTargets :: Component -> [GHC.Target]
componentTargets (CLib lib) = map targetFromModule (PD.libModules lib)
componentTargets (CExe exe) =
  targetFromSource (PD.modulePath exe) : map targetFromModule (exeModules exe)
componentTargets _ = [] -- potentially TODO

targetFromModule :: Distribution.ModuleName.ModuleName -> GHC.Target
targetFromModule m = GHC.Target (GHC.TargetModule $ toGhcModName m) False Nothing

targetFromSource :: FilePath -> GHC.Target
targetFromSource f = GHC.Target (GHC.TargetFile f Nothing) False Nothing

toGhcModName :: Distribution.ModuleName.ModuleName -> GHC.ModuleName
toGhcModName = GHC.mkModuleName . display

initBuildInfoDynFlags :: Verbosity -> LocalBuildInfo
                      -> BuildInfo -> ComponentLocalBuildInfo
                      -> DynFlags -> IO DynFlags
initBuildInfoDynFlags verbosity lbi bi clbi dflags0 = do
    debug verbosity $ "initCabalDynFlags': Flags = "++show rendered
    (dflags, leftovers, warnings) <- DynFlags.parseDynamicFlagsCmdLine dflags0 (map SrcLoc.noLoc rendered)
    putStrLn $ unlines $ map SrcLoc.unLoc warnings
    return dflags
  where
    baseOpts = CGHC.componentGhcOptions verbosity lbi bi clbi (buildDir lbi)
#if MIN_VERSION_Cabal(1,20,0)
    rendered = CGHC.renderGhcOptions (LBI.compiler lbi) baseOpts
#else
    rendered = CGHC.renderGhcOptions (Compiler.compilerVersion (LBI.compiler lbi)) baseOpts
#endif
