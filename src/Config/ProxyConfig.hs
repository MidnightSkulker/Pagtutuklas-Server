-- | Configuration information that is relevant to locales
module Config.ProxyConfig
    ( ProxyConfig (..)
    , HasProxyConfig ( .. )
    , getProxyHost
    , processProxyOptions  
    , parseProxyConfig
    , proxyConfigOptions
    ) where

-- Haskell and hackage
import Control.Monad ( liftM )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO )
import System.Console.GetOpt ( OptDescr (..), usageInfo, ArgDescr (..) )
-- Janrain
import Config.ConfigClass ( Config (..), outConfig )
import Config.Options ( Option (..), argsToOptions, die
                      , parseHostPortOption, singleParameter )
import Config.HostPort ( HostPort )

-- | Configuration information that is relevant to the log level
data ProxyConfig = ProxyConfig { proxyHostPort :: Maybe HostPort  } deriving ( Show )

class HasProxyConfig c where
  getProxyConfig :: c -> ProxyConfig

instance HasProxyConfig ProxyConfig where
  getProxyConfig = id

instance Config ProxyConfig where
  parseConfig = parseProxyConfig
  options = const proxyConfigOptions

-- | Fish the proxy host out of the proxy config
getProxyHost :: (HasProxyConfig c) => c -> Maybe HostPort
getProxyHost = proxyHostPort . getProxyConfig
  
-- | Options relevant to Proxy configuration
proxyConfigOptions :: [ OptDescr Option ]
proxyConfigOptions = [ Option "" ["proxy"] (ReqArg ProxyHostPort "HOST:PORT")
                       "The host and port of the HTTP proxy (host:port)" ]

-- | Parsing of Proxy config
processProxyOptions :: (MonadIO m, MonadError e m) =>
                     [Option] -> m ProxyConfig
processProxyOptions opts = do
  proxyPort <- singleParameter "proxy" [m | ProxyHostPort m <- opts] Nothing $ \s ->
                               Just `liftM` parseHostPortOption "proxy" s
  return ProxyConfig { proxyHostPort = proxyPort }

-- | Parse the beanstalk configuration data
parseProxyConfig :: String            -> -- ^ Version string
                    String            -> -- ^ Build String
                    String            -> -- ^ Program name
                    [String]          -> -- ^ Command ling args
                    [OptDescr Option] -> -- ^ Relevant options
                    IO ProxyConfig       -- ^ Beanstalk config
parseProxyConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os      
      info = usageInfo header os
  in case eopts of  
       Left err -> die info err
       Right opts -> do
         ecfg <- (runErrorT (processProxyOptions opts))
         case ecfg of
           Left err -> die info err
           Right cfg -> do outConfig cfg opts
                           return cfg
