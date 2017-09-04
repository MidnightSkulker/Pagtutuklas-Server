--------------------------------------------------------------------------------

import Distribution.Simple
import Distribution.Simple.Setup
import System.Posix.Files ( touchFile )
import Control.Monad ( unless )

import Distribution.PackageDescription

--------------------------------------------------------------------------------

argsSrc :: String
argsSrc = "src/Version.hs"

touchArgsModule :: Args -> BuildFlags -> IO HookedBuildInfo
touchArgsModule buildArgs _ = do
  unless ("testing" `elem` buildArgs) $ touchFile argsSrc
  return emptyHookedBuildInfo

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { preBuild = touchArgsModule }

--------------------------------------------------------------------------------
