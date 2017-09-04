{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.BaseUrl.Args
       ( parseBaseUrlConfig
       , baseUrlConfigOptions
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
import Config.MinConfig             ( processMinOptions, minConfigOptions )
import Config.RPXConfig             ( processRPXOptions, rpxConfigOptions )
import Config.DBConfig              ( processDBOptions, dbConfigOptions )
import Config.LogConfig             ( processLogOptions, logConfigOptions )
import Config.ConfigClass           ( Config (..), outConfig )
import Config.MailConfig            ( mkMailConfig )
import Config.CacheConfig           ( processCacheOptions )
import Config.Options               ( Option (..), argsToOptions, die, stringParameter )
import Config.BaseUrl.BaseUrlConfig ( BaseUrlConfig (..) )

baseUrlConfigOptions :: [ OptDescr Option ]
baseUrlConfigOptions = minConfigOptions ++ rpxConfigOptions ++
                       dbConfigOptions ++ logConfigOptions

instance Config BaseUrlConfig where
  parseConfig = parseBaseUrlConfig
  options = const baseUrlConfigOptions

-- | Parse the BaseUrl configuration data
parseBaseUrlConfig :: String -> -- ^ Version string
                      String -> -- ^ Build String
                      String -> -- ^ Program name
                      [String] -> -- ^ Command ling args
                      [OptDescr Option] -> -- ^ Relevant options
                      IO BaseUrlConfig -- ^ Beanstalk config
parseBaseUrlConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os
      info   = usageInfo header os
  in do
    putStrLn ( ">>>> parseBaseUrlConfig: " ++ show (length os) ++ "\n" )
    putStrLn ( "Options: " ++ show eopts ++ "\n\n\n" )
    case eopts of
      Left err -> do putStrLn ( "Number of errors: " ++ show (length err) )
                     die info err
      Right opts -> do
        ecfg <- (runErrorT (processBaseUrlOptions opts))
        case ecfg of
          Left err -> die info err
          Right cfg -> do outConfig cfg opts
                          return cfg

processBaseUrlOptions :: (MonadIO m, MonadError e m) => [Option] -> m BaseUrlConfig
processBaseUrlOptions opts = do
  liftIO $ putStrLn ( ">>>> processBaseUrlOptions\n" )
  minCfg     <- processMinOptions opts
  liftIO $ putStrLn ( "---- processBaseUrlOptions minCfg = " ++ show minCfg )
  cacheCfg <- processCacheOptions opts
  liftIO $ putStrLn ( "---- processBaseUrlOptions cacheCfg = " ++ show cacheCfg)
  logCfg     <- processLogOptions opts
  liftIO $ putStrLn ( "---- processBaseUrlOptions logCfg = " ++ show logCfg )  
  dbCfg      <- processDBOptions opts
  liftIO $ putStrLn ( "---- processBaseUrlOptions dbCfg = " ++ show dbCfg )
  defMailDomain <-
      stringParameter "mail-domain" [m | MailDomain m <- opts] "rpxnow.com"

  defMailServer <-
      stringParameter "mail-server" [m | MailServer m <- opts] "aspmx.l.google.com"

  eitherMailConfig <- liftIO $ catch (Right <$> fromJust <$> mkMailConfig defMailServer defMailDomain)
                      (return . Left . showMailConfigError defMailServer defMailDomain)
  liftIO $ putStrLn ( "---- processBaseUrlOptions defMailDomain = " ++ show defMailDomain )
  liftIO $ putStrLn ( "---- processBaseUrlOptions defMailServer = " ++ show defMailServer )
  liftIO $ putStrLn ( "---- processBaseUrlOptions eitherMailConfig = " ++ show eitherMailConfig )
  mailCfg <- either fail return eitherMailConfig
  liftIO $ putStrLn ( "---- processBaseUrlOptions done with mail config stuff\n" )
  rpxCfg <- processRPXOptions opts
  liftIO $ putStrLn ( "---- processBaseUrlOptions rpxCfg = " ++ show rpxCfg )  
  return BaseUrlConfig { minConfig     = minCfg
                       , logConfig     = logCfg
                       , rpxConfig     = rpxCfg
                       , cacheConfig   = cacheCfg
                       , dbConfig      = dbCfg
                       , mailConfig    = mailCfg
                       }

showMailConfigError :: String -> String -> IOError -> String
showMailConfigError server domain e =
    "Error configuring mail server " ++
    show server ++ ", domain " ++ show domain ++ ": " ++ show e
