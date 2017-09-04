-- | Configuration information that is relevant to RPX Protocols
module Config.RPXConfig
    ( RPXConfig (..)
    , HasRPXConfig ( .. )
    , getRPXHost
    , getDefaultProto
    , HostPort
    , processRPXOptions
    , parseRPXConfig
    , rpxConfigOptions
    ) where

-- Haskell and hackage
import Data.Char ( toLower )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO )
import System.Console.GetOpt
-- Janrain
import Config.ConfigClass ( Config (..), outConfig )
import Config.Options ( Option (..), argsToOptions, die, singleParameter 
                      , stringParameter )
import Config.HostPort ( HostPort )

-- | Configuration information that is relevant to the log level
data RPXConfig = RPXConfig
    { rpxHost       :: String
    , defaultProto  :: String
    } deriving ( Show )

class HasRPXConfig c where
  getRPXConfig :: c -> RPXConfig

instance HasRPXConfig RPXConfig where
  getRPXConfig = id

instance Config RPXConfig where
  parseConfig = parseRPXConfig
  options = const rpxConfigOptions

-- | Options relevant to RPX configuration
rpxConfigOptions :: [ OptDescr Option ]
rpxConfigOptions =
  [ Option "r" ["default-protocol"] (ReqArg DefaultProtocol "(http|https)")
    "What protocol to use for the redirect if it can't be detected \\(default: https)"
  ]

-- | Fish the RPX host out of the RPX config
getRPXHost :: (HasRPXConfig c) => c -> String
getRPXHost = rpxHost . getRPXConfig

-- | Fish the default protocol out of the RPX config
getDefaultProto :: (HasRPXConfig c) => c -> String
getDefaultProto = defaultProto . getRPXConfig

-- | Parse the RPX options
processRPXOptions :: (MonadIO m, MonadError e m) => [Option] -> m RPXConfig
processRPXOptions opts = do
  defProto <-
      singleParameter "default-protocol" [m | DefaultProtocol m <- opts]
                          "https" $ \s ->
      let p = map toLower s
      in if p `elem` ["http", "https"]
         then return p
         else fail $ "Expected 'http' or 'https' for protocol. Got: " ++ show s
  defRpxHost <-
      stringParameter "rpx-host" [m | RpxHost m <- opts] "rpxnow.com"
  return RPXConfig { defaultProto = defProto
                   , rpxHost = defRpxHost                         
                   }

-- | Parse the RPX configuration data
parseRPXConfig :: String -> -- ^ Version string
                  String -> -- ^ Build String
                  String -> -- ^ Program name
                  [String] -> -- ^ Command ling args
                  [OptDescr Option] -> -- ^ Relevant options
                  IO RPXConfig -- ^ Beanstalk config
parseRPXConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os      
      info = usageInfo header os
  in case eopts of
       Left err -> die info err
       Right opts -> do
         ecfg <- (runErrorT (processRPXOptions opts))
         case ecfg of
           Left err -> die info err
           Right cfg -> do outConfig cfg opts
                           return cfg
