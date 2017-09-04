{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.RpxMe.Args
       ( parseRpxMeConfig
       , rpxMeConfigOptions
       ) where

-- Haskell and hackage
import Prelude hiding ( catch )
import Control.Exception ( catch )
import Control.Applicative ( (<$>) )
import System.Console.GetOpt
import Control.Monad.Error( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Maybe ( fromJust )
-- Janrain
import Config.RpxMe.RpxMeConfig ( RpxMeConfig (..) )
import Config.MinConfig         ( processMinOptions, minConfigOptions )
import Config.RPXConfig         ( processRPXOptions, rpxConfigOptions )
import Config.DBConfig          ( processDBOptions, dbConfigOptions )
import Config.BeanstalkConfig   ( processBeanstalkOptions, beanstalkConfigOptions )
import Config.LogConfig         ( processLogOptions, logConfigOptions )
import Config.ConfigClass       ( Config (..), outConfig )
import Config.MailConfig        ( mkMailConfig )
import Config.Options           ( Option (..), argsToOptions, die, stringParameter )

rpxMeConfigOptions :: [ OptDescr Option ]
rpxMeConfigOptions = minConfigOptions ++ dbConfigOptions ++ rpxConfigOptions ++ 
                     logConfigOptions ++ beanstalkConfigOptions
instance Config RpxMeConfig where
  parseConfig = parseRpxMeConfig
  options = const rpxMeConfigOptions

-- | Parse the beanstalk configuration data
parseRpxMeConfig :: String -> -- ^ Version string
                    String -> -- ^ Build String
                    String -> -- ^ Program name
                    [String] -> -- ^ Command ling args
                    [OptDescr Option] -> -- ^ Relevant options
                    IO RpxMeConfig -- ^ Beanstalk config
parseRpxMeConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os
      info   = usageInfo header os
  in do
    case eopts of
      Left err -> do putStrLn ( "Number of errors: " ++ show (length err) )
                     die info err
      Right opts -> do
        ecfg <- (runErrorT (processRpxMeOptions opts))
        case ecfg of
          Left err -> die info err
          Right cfg -> do outConfig cfg opts
                          return cfg

processRpxMeOptions :: (MonadIO m, MonadError e m) => [Option] -> m RpxMeConfig
processRpxMeOptions opts = do
  minCfg     <- processMinOptions opts
  logCfg     <- processLogOptions opts
  beanstalkCfg <- processBeanstalkOptions opts
  dbCfg      <- processDBOptions opts
  defMailDomain <-
      stringParameter "mail-domain" [m | MailDomain m <- opts] "rpxnow.com"

  defMailServer <-
      stringParameter "mail-server" [m | MailServer m <- opts] "aspmx.l.google.com"

  eitherMailConfig <- liftIO $ catch (Right <$> fromJust <$> mkMailConfig defMailServer defMailDomain)
                      (return . Left . showMailConfigError defMailServer defMailDomain)
  mailCfg <- either fail return eitherMailConfig
  rpxCfg <- processRPXOptions opts
  return RpxMeConfig { minConfig       = minCfg
                     , logConfig       = logCfg
                     , rpxConfig       = rpxCfg
                     , dbConfig        = dbCfg
                     , mailConfig      = mailCfg
                     , beanstalkConfig = beanstalkCfg                     
                     }

showMailConfigError :: String -> String -> IOError -> String
showMailConfigError server domain e =
    "Error configuring mail server " ++
    show server ++ ", domain " ++ show domain ++ ": " ++ show e
