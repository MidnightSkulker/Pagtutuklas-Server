module BuildInfo
    ( getPkgVersionString
    , getBuildDepends
    , getBuildDate
    , getBuildHost
    , getBuildUser
    , getBuildInfoString
    ) where

import System.FilePath ( splitExtension, combine )
import System.Directory ( getDirectoryContents )
import System.Environment ( getEnv )
import Language.Haskell.TH
import Distribution.Verbosity
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Simple.Configure
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Data.Version
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ()
import Control.Monad ( liftM, liftM4 )
import Network.BSD ( getHostName )
import Text.Printf

-- Get the parsed .cabal file for this package
getPkgDesc :: Q String -> Q GenericPackageDescription
getPkgDesc getRoot = do
  repoRoot <- getRoot

--  let isCabalFile = splitExtension >>> snd >>> (== ".cabal")
  let -- Determine when a file is a cabal file by checking if the
      -- file extension is ".cabal"
      isCabalFile :: String -> Bool
      isCabalFile s = snd ( splitExtension s ) == ".cabal"
  [cabalFileName] <-
      runIO $ filter isCabalFile `liftM` getDirectoryContents repoRoot

  let cabalFilePath = combine repoRoot cabalFileName

  runIO $ readPackageDescription silent cabalFilePath

-- Get the name and version string for the current package
getPackageIdentifier :: Q String -> Q PackageIdentifier
getPackageIdentifier getRoot =
  (package . packageDescription) `liftM` (getPkgDesc getRoot)

showPackageIdentifier :: PackageIdentifier -> String
showPackageIdentifier pkgId =
    let ver = showVersion $ pkgVersion pkgId
        PackageName name = pkgName pkgId
    in name ++ "-" ++ ver

-- Return an expression for the package name combined with the package version
-- e.g. "discovery-server-0.1"
getPkgVersionString :: Q String -> Q String
getPkgVersionString getRoot =
  showPackageIdentifier `liftM` (getPackageIdentifier getRoot)

getBuildDate :: Q String
getBuildDate = runIO (show `liftM` getCurrentTime)

getBuildHost :: Q String
getBuildHost = runIO getHostName

getBuildUser :: Q String
getBuildUser = runIO (getEnv "USER")

getBuildInfoString :: Q String -> Q String
getBuildInfoString getRoot =
    liftM4 (printf "%s: Built by %s@%s at %s")
    (getPkgVersionString getRoot) getBuildUser getBuildHost getBuildDate

getBuildDepends :: Q String -> Q String
getBuildDepends getRoot = do
  repoRoot <- getRoot

  let setupInfoPath = combine repoRoot "dist"
  mBuildConfig <- runIO $ maybeGetPersistBuildConfig setupInfoPath
  let buildDeps =
          case mBuildConfig of
            Nothing -> "Cabal configuration not found at: " ++ setupInfoPath
            Just buildConfig ->
                let compilerStr = printf "Built by %s with packages:\n" $
                                  showCompilerId $ compiler buildConfig
                    showDep pkgId = "  " ++ showPackageIdentifier pkgId ++ "\n"
                    depsStr = concatMap (showDep . snd) $ externalPackageDeps buildConfig
                in compilerStr ++ depsStr

  return buildDeps
