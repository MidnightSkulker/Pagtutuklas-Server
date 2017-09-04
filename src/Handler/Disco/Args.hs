{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Disco.Args
       ( parseDiscoConfig
       , discoConfigOptions
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
import Config.MinConfig         ( processMinOptions, minConfigOptions )
import Config.RPXConfig         ( processRPXOptions, rpxConfigOptions )
import Config.ProxyConfig       ( processProxyOptions, proxyConfigOptions )
import Config.LogConfig         ( processLogOptions, logConfigOptions )
import Config.ConfigClass       ( Config (..), outConfig )
import Config.MailConfig        ( mkMailConfig )
import Config.Options           ( Option (..), argsToOptions, die, stringParameter )
import Config.Disco.DiscoConfig ( DiscoConfig (..) )

discoConfigOptions :: [ OptDescr Option ]
discoConfigOptions = minConfigOptions ++ rpxConfigOptions ++ logConfigOptions ++
                     proxyConfigOptions

instance Config DiscoConfig where
  parseConfig = parseDiscoConfig
  options = const discoConfigOptions

-- | Parse the Disco configuration data
parseDiscoConfig :: String -> -- ^ Version string
                    String -> -- ^ Build String
                    String -> -- ^ Program name
                    [String] -> -- ^ Command ling args
                    [OptDescr Option] -> -- ^ Relevant options
                    IO DiscoConfig -- ^ Beanstalk config
parseDiscoConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os
      info   = usageInfo header os
  in do
    putStrLn ( ">>>> parseDiscoConfig: " ++ show (length os) ++ "\n" )
    putStrLn ( "Options: " ++ show eopts ++ "\n\n\n" )
    case eopts of
      Left err -> do putStrLn ( "Number of errors: " ++ show (length err) )
                     die info err
      Right opts -> do
        ecfg <- (runErrorT (processDiscoOptions opts))
        case ecfg of
          Left err -> die info err
          Right cfg -> do outConfig cfg opts
                          return cfg

processDiscoOptions :: (MonadIO m, MonadError e m) => [Option] -> m DiscoConfig
processDiscoOptions opts = do
  liftIO $ putStrLn ( ">>>> processDiscoOptions\n" )
  proxyCfg   <- processProxyOptions opts
  liftIO $ putStrLn ( "---- processDiscoOptions proxyCfg = " ++ show proxyCfg )
  minCfg     <- processMinOptions opts
  liftIO $ putStrLn ( "---- processDiscoOptions minCfg = " ++ show minCfg )
  logCfg     <- processLogOptions opts
  liftIO $ putStrLn ( "---- processDiscoOptions logCfg = " ++ show logCfg )  
  defMailDomain <-
      stringParameter "mail-domain" [m | MailDomain m <- opts] "rpxnow.com"

  defMailServer <-
      stringParameter "mail-server" [m | MailServer m <- opts] "aspmx.l.google.com"

  eitherMailConfig <- liftIO $ catch (Right <$> fromJust <$> mkMailConfig defMailServer defMailDomain)
                      (return . Left . showMailConfigError defMailServer defMailDomain)
  liftIO $ putStrLn ( "---- processDiscoOptions defMailDomain = " ++ show defMailDomain )
  liftIO $ putStrLn ( "---- processDiscoOptions defMailServer = " ++ show defMailServer )
  liftIO $ putStrLn ( "---- processDiscoOptions eitherMailConfig = " ++ show eitherMailConfig )
  mailCfg <- either fail return eitherMailConfig
  liftIO $ putStrLn ( "---- processDiscoOptions done with mail config stuff\n" )
  rpxCfg <- processRPXOptions opts
  liftIO $ putStrLn ( "---- processDiscoOptions rpxCfg = " ++ show rpxCfg )  
  return DiscoConfig { minConfig     = minCfg
                     , logConfig     = logCfg
                     , rpxConfig     = rpxCfg
                     , mailConfig    = mailCfg
                     , proxyConfig   = proxyCfg
                     }

showMailConfigError :: String -> String -> IOError -> String
showMailConfigError server domain e =
    "Error configuring mail server " ++
    show server ++ ", domain " ++ show domain ++ ": " ++ show e
