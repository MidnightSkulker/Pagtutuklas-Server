-- | Configuration information that is relevant to
--   the beanstalk server.
module Config.BeanstalkConfig
  ( BeanstalkConfig (..)
  , HasBeanstalkConfig (..)
  , getBeanstalkHostPort
  , getBsPoolSize
  , getBsTubeName
  , getBsJobLimit
  , getBsOverflowDir
  , processBeanstalkOptions
  , parseBeanstalkConfig
  , beanstalkConfigOptions  
  ) where

-- Haskell and Hackage
import System.Console.GetOpt ( OptDescr )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Arrow ( second )
import System.Console.GetOpt ( OptDescr (..), usageInfo, ArgDescr (..) )
import Network.BSD ( getHostByName )
-- Janrain
import Config.ConfigClass ( Config (..), outConfig, toArgString, optionArgs )
import Config.Options ( Option (..), argsToOptions, singleParameter 
                      , hostPortOption, stringParameter, die )
import Config.HostPort ( HostPort (..) )

-- | Options relevant to Beanstalk configuration
beanstalkConfigOptions :: [ OptDescr Option ]
beanstalkConfigOptions =
  [ Option "" ["beanstalk-pool-size"] (ReqArg BsPoolSize "SIZE")
    "The number of connections to put in the beanstalk connection pool"
  , Option "" ["beanstalk-tube-name"] (ReqArg BsTubeName "NAME")
    "The beanstalk tube to use instead of \"default\""
  , Option "" ["beanstalk-job-limit"] (ReqArg BsJobLimit "NUMBER")
    "Max beanstalk tube size; overflow goes to the overflow directory"
  , Option "" ["beanstalk-overflow-dir"] (ReqArg BsOverflowDir "DIRECTORY")
    "The directory where to save overflow beanstalk jobs"
  ]

-- | Configuration information relevant to the beanstalk server.
-- Creating the beanstalk config also requires the DB setting
data BeanstalkConfig = BeanstalkConfig
  { beanstalkHostPort :: HostPort
  , bsPoolSize        :: Int
  , bsTubeName        :: String
  , bsJobLimit        :: Int
  , bsOverflowDir     :: String
  }

instance HasBeanstalkConfig BeanstalkConfig where
  getBeanstalkConfig = id

instance Config BeanstalkConfig where
  parseConfig = parseBeanstalkConfig
  options     = const beanstalkConfigOptions
 
-- | Class characterizing having beanstalk configuraton information
class HasBeanstalkConfig c where
  getBeanstalkConfig :: c -> BeanstalkConfig

getBeanstalkHostPort :: HasBeanstalkConfig c => c -> HostPort
getBeanstalkHostPort c = beanstalkHostPort ( getBeanstalkConfig c )

getBsPoolSize :: HasBeanstalkConfig c => c -> Int
getBsPoolSize c = bsPoolSize ( getBeanstalkConfig c )
  
getBsTubeName :: HasBeanstalkConfig c => c -> String
getBsTubeName c = bsTubeName ( getBeanstalkConfig c )

getBsJobLimit :: HasBeanstalkConfig c => c -> Int
getBsJobLimit c = bsJobLimit ( getBeanstalkConfig c )

getBsOverflowDir :: HasBeanstalkConfig c => c -> String
getBsOverflowDir c = bsOverflowDir ( getBeanstalkConfig c )

-- | Parsing of Beanstalk config
processBeanstalkOptions :: (MonadIO m, MonadError e m) =>
                           [Option] -> m BeanstalkConfig
processBeanstalkOptions opts = do
  defaultHostEntry <- liftIO $ getHostByName "127.0.0.1"  
  bsHostPort <- hostPortOption "beanstalk-hostport"
                  [m | BeanstalkHostPort m <- opts]
                  HostPort { hostEntry = defaultHostEntry
                           , hostPort  = 11300
                           , hostAddr  = "127.0.0.1"
                           }

  defBsPoolSize <-
      singleParameter "beanstalk-pool-size" [m | BsPoolSize m <- opts]
                          10 (return . read)

  defBsTubeName <-
      stringParameter "beanstalk-tube-name" [m | BsTubeName m <- opts] "default"

  defBsJobLimit <- singleParameter "beanstalk-job-limit"
                                   [m | BsJobLimit m <- opts] 1024 (return . read)

  maybeBsOverflowDir <-
      singleParameter "beanstalk-overflow-dir"
                      [m | BsOverflowDir m <- opts] Nothing (return . Just)

  defBsOverflowDir <-
          case maybeBsOverflowDir of
              Nothing -> fail "Expected non-empty overflow directory name."
              Just "" -> fail "Expected non-empty overflow directory name."
              Just dir -> return dir

  return BeanstalkConfig { beanstalkHostPort = bsHostPort
                         , bsPoolSize = defBsPoolSize
                         , bsTubeName = defBsTubeName
                         , bsOverflowDir = defBsOverflowDir
                         , bsJobLimit = defBsJobLimit
                         }
    
-- | Parse the beanstalk configuration data
parseBeanstalkConfig :: String -> -- ^ Version string
                        String -> -- ^ Build String
                        String -> -- ^ Program name
                        [String] -> -- ^ Command ling args
                        [OptDescr Option] -> -- ^ Relevant options
                        IO BeanstalkConfig -- ^ Beanstalk config
parseBeanstalkConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os
      info   = usageInfo header os
  in case eopts of
          Left err -> die info err
          Right opts -> do
              ecfg <- (runErrorT (processBeanstalkOptions opts))
              case ecfg of
                Left err -> die info err
                Right cfg -> do outConfig cfg opts
                                return cfg

instance Show BeanstalkConfig where
  show cfg =
    let otherArgs = [ ("beanstalk-pool-size", show $ getBsPoolSize cfg)
                    , ("beanstalk-tube-name", show $ getBsTubeName cfg)
                    , ("beanstalk-job-limit", show $ getBsJobLimit cfg)
                    , ("beanstalk-overflow-dir", show $ getBsOverflowDir cfg)
                    ]
        hostPortArgs = [ ("beanstalk-hostport", getBeanstalkHostPort cfg) ]
        argPairs = otherArgs ++ map (second show) hostPortArgs
    in toArgString $ optionArgs argPairs
