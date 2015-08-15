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

import Control.Applicative ((<|>))
import Distribution.Verbosity
import Distribution.Simple.Utils (defaultPackageDesc, warn, debug, findPackageDesc)
import Distribution.Simple.Program (defaultProgramConfiguration)
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
    , cdComponentName           :: ComponentName
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
    let comp :: Maybe (PD.BuildInfo, ComponentName, [GHC.Target])
        comp = msum [libraryComp, executableComp]

        libraryComp = do
            lib <- PD.library pkg_descr
            let compName = CLibName
                bi = PD.libBuildInfo lib
            guard $ PD.buildable bi
            let targets = map (\m -> GHC.Target (GHC.TargetModule $ toGhcModName m) False Nothing)
                              (PD.exposedModules lib)
            return (bi, compName, targets)

        executableComp = msum $ flip map (PD.executables pkg_descr) $ \exec->do
            let compName = LBI.CExeName $ PD.exeName exec
                bi = PD.buildInfo exec
            guard $ PD.buildable bi
            let targets = [GHC.Target (GHC.TargetFile (PD.modulePath exec) Nothing) False Nothing]
            return (bi, compName, targets)

    let warnNoComps = do
            lift $ warn verbosity $ "Found no buildable components in "++pdfile
            mzero
    (bi, compName, targets) <- maybe warnNoComps pure comp
    let clbi = getComponentLocalBuildInfo lbi compName

    dflags <- lift $ initBuildInfoDynFlags verbosity lbi bi clbi dflags0
    let cd = CabalDetails { cdPackageDescription      = pkg_descr
                          , cdLocalBuildInfo          = lbi
                          , cdComponentName           = compName
                          , cdComponentLocalBuildInfo = clbi
                          , cdTargets                 = targets
                          }
    return (dflags, cd)

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
