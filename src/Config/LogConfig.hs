-- | Configuration information that is relevant to locales
module Config.LogConfig
    ( LogConfig (..)
    , HasLogConfig ( .. )
    , getLogLevel
    , getEmailFromAddr
    , getEmailToAddrs
    , processLogOptions  
    , parseLogConfig
    , logConfigOptions
    ) where

-- Haskell and hackage
import Data.List ( intercalate )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO )
import System.Console.GetOpt ( OptDescr (..), usageInfo, ArgDescr (..) )
-- Janrain
import Config.ConfigClass ( Config (..), outConfig, toArgString, optionArgs )
import Config.Options ( Option (..), argsToOptions, die, stringParameter )
import Config.MailConfig ( ToAddress, FromAddress )
import Config.Level ( Level (..) )

-- | Configuration information that is relevant to the log level
data LogConfig = LogConfig
    { logLevel         :: Level 
    , emailFromAddr    :: FromAddress
    , emailToAddrs     :: [ ToAddress ]
    }

class HasLogConfig c where
  getLogConfig :: c -> LogConfig

instance HasLogConfig LogConfig where
  getLogConfig = id

instance Config LogConfig where
  parseConfig = parseLogConfig
  options = const logConfigOptions
  
-- | Options relevant to Log configuration
logConfigOptions :: [ OptDescr Option ]
logConfigOptions =
  [ Option "" ["mail-server"] (ReqArg MailServer "SERVER")
    "The SMTP server address to use when sending e-mail"
  , Option "" ["mail-domain"] (ReqArg MailDomain "MAIL-DOMAIN")
    "The domain to use in the SMTP EHLO"
  , Option "" ["mail-from"] (ReqArg MailFrom "ADDRESS")
    "Source address for e-mail messages (default: tracebacks@MAIL-DOMAIN)"
  , Option "" ["mail-to"] (ReqArg MailTo "ADDRESS")
    "Destination address for e-mail messages (repeat option to add addresses)"
  ]

-- | Fish the log level out of the log config
getLogLevel :: (HasLogConfig c) => c -> Level
getLogLevel = logLevel . getLogConfig

-- | Fish the from address out of the log config
getEmailFromAddr :: (HasLogConfig c) => c -> FromAddress
getEmailFromAddr = emailFromAddr . getLogConfig

-- | Fish the ts addresses out of the log config
getEmailToAddrs :: (HasLogConfig c) => c -> [ ToAddress ]
getEmailToAddrs = emailToAddrs . getLogConfig

-- | Parsing of Log config
processLogOptions :: (MonadIO m, MonadError e m) =>
                     [Option] -> m LogConfig
processLogOptions opts = do
  let defLogLevel = case length $ filter (== VerboseLogging) opts of
                      0 -> Normal
                      1 -> Verbose
                      _ -> Debug
      defMailTo = [m | MailTo m <- opts]  
  defMailDomain <-
      stringParameter "mail-domain" [m | MailDomain m <- opts] "rpxnow.com"
  defMailFrom <-
      stringParameter "mail-from" [m | MailFrom m <- opts] $
                         "tracebacks@" ++ defMailDomain
  return LogConfig { logLevel = defLogLevel
                   , emailFromAddr = defMailFrom
                   , emailToAddrs = defMailTo
                   }

-- | Parse the beanstalk configuration data
parseLogConfig :: String -> -- ^ Version string
                  String -> -- ^ Build String
                  String -> -- ^ Program name
                  [String] -> -- ^ Command ling args
                  [OptDescr Option] -> -- ^ Relevant options
                  IO LogConfig -- ^ Beanstalk config
parseLogConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os      
      info = usageInfo header os
  in case eopts of  
       Left err -> die info err
       Right opts -> do
         ecfg <- (runErrorT (processLogOptions opts))
         case ecfg of
           Left err -> die info err
           Right cfg -> do outConfig cfg opts
                           return cfg

instance Show LogConfig where
  show cfg =
    let otherArgs = [ ("mail-to", intercalate "," $ getEmailToAddrs cfg)
                    , ("mail-from", getEmailFromAddr cfg)
                    ]
        verbose = replicate (case getLogLevel cfg of
                               Normal -> 0
                               Verbose -> 1
                               Debug -> 2) "verbose"
    in toArgString $ verbose ++ optionArgs otherArgs
