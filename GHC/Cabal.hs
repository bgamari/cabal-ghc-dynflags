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
-- >     (dflags', _) <- fromMaybe dflags <$> liftIO (initCabalDynFlags Verbosity.normal dflags)
-- >     GHC.setSessionDynFlags dflags'
-- >
-- >     -- Standard GHC API usage goes here
--
-- In addition to the 'DynFlags', 'initCabalDynFlags' also offers a variety of
-- information about the current project in the form of 'CabalDetails'. Perhaps
-- the most useful information offered is 'cdTargets', which lists the source files
-- of the currently selected Cabal component.
--
-- For instance, to automatically bring project's module's into 
--
-- > (dflags', cd) <- maybe (dflags, Nothing) (\(a,b)->(a, Just b))
-- >                  <$> liftIO (initCabalDynFlags (verbose args) dflags)
-- > GHC.setSessionDynFlags dflags'
-- > traverse (GHC.setTargets . cdTargets) cd
--

module GHC.Cabal (
      -- * Initializing GHC DynFlags for Cabal packages
      initCabalDynFlags
    , CabalDetails(..)
    ) where

import Control.Monad (guard, msum, mzero)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class
import Data.Maybe (listToMaybe)

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
      -- | The GHC targets representing the sources of the current component.
      -- What this means depends upon what type of component has been selected:
      --
      --    * library: libraries these are the library's exposed modules
      --    * executable: the source of the executable's @Main@ source file
    , cdTargets                 :: [GHC.Target]
    }

-- | Modify a set of 'DynFlags' to match what Cabal would produce.
initCabalDynFlags :: Verbosity -> DynFlags -> IO (Maybe (DynFlags, CabalDetails))
initCabalDynFlags verbosity dflags0 = runMaybeT $ do
    let warnNoCabal _err = lift (warn verbosity "Couldn't find cabal file") >> mzero
    pdfile <- either warnNoCabal pure =<< lift (findPackageDesc ".")
    gpkg_descr <- lift $ PD.readPackageDescription verbosity pdfile
    lbi <- lift $ Configure.getPersistBuildConfig Setup.defaultDistPref

    let programsConfig = defaultProgramConfiguration
    (comp, compPlatform, programsConfig') <- lift $
        Configure.configCompilerEx (Just Compiler.GHC) Nothing Nothing
                                   (withPrograms lbi) (lessVerbose verbosity)

    -- TODO: is any of this correct?
    let pkg_descr = case finalizePackageDescription
                             [] (const True) compPlatform (Compiler.compilerInfo comp)
                             [] gpkg_descr of
                        Right (pd,_) -> pd
                        -- This shouldn't happen since we claim dependencies can always be satisfied
                        Left err     -> error "missing dependencies"

    let warnNoComps = do
            lift $ warn verbosity $ "Found no buildable components in "++pdfile
            mzero
    comp <- maybe warnNoComps pure $ listToMaybe $ LBI.pkgEnabledComponents pkg_descr
    let bi = LBI.componentBuildInfo comp
        compName = LBI.componentName comp
        clbi = getComponentLocalBuildInfo lbi compName
        targets = componentTargets comp

    dflags <- lift $ initBuildInfoDynFlags verbosity lbi bi clbi dflags0
    let cd = CabalDetails { cdPackageDescription      = pkg_descr
                          , cdLocalBuildInfo          = lbi
                          , cdComponent               = comp
                          , cdComponentLocalBuildInfo = clbi
                          , cdTargets                 = targets
                          }
    return (dflags, cd)

-- | The GHC targets associated with a 'Component'.
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
    rendered = CGHC.renderGhcOptions (LBI.compiler lbi) baseOpts
