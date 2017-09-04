module Config.MinConfig
       ( HasMinConfig (..)
       , MinConfig (..)
       , getMaxConnections
       , getListen
       , parseMinConfig
       , processMinOptions
       , minConfigOptions
       ) where

-- Haskell and Hackage
import Control.Arrow ( second )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO, liftIO )
import System.Console.GetOpt ( OptDescr (..), usageInfo, ArgDescr (..) )
import Network.BSD ( getHostByName )
-- Janrain
import Config.HostPort ( HostPort (..) )
import Config.ConfigClass ( Config (..), optionArgs, toArgString, outConfig )
import Config.Options ( Option (..), argsToOptions, hostPortOption, die
                      , singleParameter )

minConfigOptions :: [ OptDescr Option ]
minConfigOptions =
  [ Option "l" ["listen"] (ReqArg Port "HOST:PORT")
           "address to listen on (default: localhost:4001)"
  , Option "c" ["max-connections"] (ReqArg MaxConnections "CONNECTIONS")
           "The maximum number of simultaneous connections to allow"
  ]

class HasMinConfig c where
  getMinConfig :: c -> MinConfig

-- | Minimum configuration data required for a handler
data MinConfig = MinConfig { listen         :: HostPort  -- ^ Host name and port
                           , maxConnections :: Maybe Int -- ^ Max user connections
                           }

instance HasMinConfig MinConfig where
  getMinConfig = id
  
instance Config MinConfig where
  parseConfig = parseMinConfig
  options = const minConfigOptions

-- | Fish max connections out of global config
getMaxConnections :: (HasMinConfig c) => c -> Maybe Int
getMaxConnections c = maxConnections ( getMinConfig c )

-- | Fish listen out of global config
getListen :: (HasMinConfig c) => c -> HostPort
getListen c = listen ( getMinConfig c )

instance Show MinConfig where
  show cfg =
    let hostPortArgs = [ ("listen", getListen cfg) ]
        special = case getMaxConnections cfg of
                    Nothing -> []
                    Just i -> [("max-connections", show i)]
        argPairs = special ++ map (second show) hostPortArgs
    in toArgString $ optionArgs argPairs

-- | Parse the minimum configuration data
parseMinConfig :: String -> -- ^ Version string
                  String -> -- ^ Build String
                  String -> -- ^ Program name
                  [String] -> -- ^ Command ling args
                  [OptDescr Option] -> -- ^ Relevant options
                  IO MinConfig -- ^ Beanstalk config
parseMinConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os      
      info = usageInfo header os
  in case eopts of
       Left err -> die info err
       Right opts -> do
         ecfg <- (runErrorT (processMinOptions opts))
         case ecfg of
           Left err  -> die info err
           Right cfg -> do outConfig cfg opts
                           return cfg
  
-- | Parsing of MinConfig data  
processMinOptions :: (MonadIO m, MonadError e m) => [Option] -> m MinConfig
processMinOptions opts = do
  defaultHostEntry <- liftIO $ getHostByName "127.0.0.1"
  cListen <- hostPortOption "Port" [n | Port n <- opts]
             HostPort { hostPort = 4001
                      , hostEntry = defaultHostEntry
                      , hostAddr = "127.0.0.1"
                      }

  defMaxConns <- singleParameter "max-connections" [m | MaxConnections m <- opts] Nothing (return . Just . read)
  
  return MinConfig { listen = cListen 
                   , maxConnections = defMaxConns
                   }
