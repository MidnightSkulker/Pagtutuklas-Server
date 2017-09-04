-- | Module defining an arbitrary configuration class
module Config.ConfigClass
       ( Config (..) 
       , toArgString
       , optionArgs
       , outConfig
       ) where

-- Haskell and Hackage
import Data.List ( intercalate )
import Text.Printf ( printf )
import Control.Monad ( when )
import System.IO ( hPutStrLn, stderr)
import System.Exit ( ExitCode (..), exitWith )
import System.Environment ( getProgName )
import System.Console.GetOpt ( OptDescr )
-- Janrain
import Config.Options ( Option (..) )

-- | A class that characterizes configuration for a handler
class (Show c) => Config c where
  -- | The parser takes a version string and a build string and a progran
  --   name string as input and then a list of arguments as input,
  --   and returns the config structure if successful, otherwise it will
  --   die with an error message
  parseConfig :: String -> String -> String -> [String] ->  [OptDescr Option] -> IO c
  -- | The command line options relevant to the configuration information
  options     :: c -> [OptDescr Option]

-- | Used in formatting may configs for output
toArgString :: [String] -> String
toArgString = intercalate " " . map ("--" ++)

optionArgs :: [(String, String)] -> [String]
optionArgs = map (uncurry $ printf "%s=%s")

-- | Print out config if requested as a parameter
outConfig :: (Config c) => c -> [Option] -> IO ()
outConfig cfg opts = do
  pname <- getProgName
  when (ShowConfig `elem` opts) $ do
    hPutStrLn stderr $ pname ++ " " ++ show cfg
    exitWith ExitSuccess
