-- | Configuration information that is relevant to the connection to
--   the database
module Config.DBConfig
       ( DBConfig (..)
       , HasDBConfig (..)
       , getDBSettings
       , getDBConnPoolSize
       , processDBOptions
       , parseDBConfig
       , dbConfigOptions
       ) where

import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO )
import System.Console.GetOpt ( OptDescr (..), usageInfo, ArgDescr (..) )
import Config.ConfigClass ( Config (..), outConfig )
import Config.Options ( Option (..), argsToOptions, die, singleParameter
                      , stringParameter )

-- | Options relevant to DB configuration
dbConfigOptions :: [ OptDescr Option ]
dbConfigOptions =
  [ Option "" ["db-conn-string"] (ReqArg DbConnString "CONNECTION-STRING")
    "The database connection string, e.g., 'dbname=mydb user=murphy'"
  , Option "" ["db-conn-pool-size"] (ReqArg DbConnPoolSize "SIZE")
    "The number of connections to put in the database connection pool"
  ]
  
-- | Configuration information relevant to the connection to the
--   database
data DBConfig = DBConfig { dbConnPoolSize :: Int  
                         , dbSettings :: String
                         } deriving ( Show )

instance HasDBConfig DBConfig where
  getDBConfig = id

instance Config DBConfig where
  parseConfig = parseDBConfig
  options = const dbConfigOptions

-- | Class characterizing having DB config
class HasDBConfig c where
  getDBConfig :: c -> DBConfig
  
-- | Fish the DB settings out of the config
getDBSettings :: (HasDBConfig c) => c -> String
getDBSettings = dbSettings . getDBConfig

-- | Fish the DB connection pool size out of the config
getDBConnPoolSize :: (HasDBConfig c) => c -> Int
getDBConnPoolSize = dbConnPoolSize . getDBConfig

-- | Parsing of DB Options
processDBOptions :: (MonadIO m, MonadError e m) => [Option] -> m DBConfig
processDBOptions opts = do
  defPoolSize <- 
    singleParameter "db-conn-pool-size" [m | DbConnPoolSize m <- opts]
                      10 (return . read)
  defDbConnString <-
      stringParameter "db-conn-string" [m | DbConnString m <- opts] ""
  return DBConfig { dbSettings = defDbConnString
                  , dbConnPoolSize = defPoolSize
                  }

-- | Parse the RPX configuration data
parseDBConfig :: String            -> -- ^ Version string
                 String            -> -- ^ Build String
                 String            -> -- ^ Program name
                 [String]          -> -- ^ Command ling args
                 [OptDescr Option] -> -- ^ Relevant options
                 IO DBConfig          -- ^ Beanstalk config
parseDBConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os      
      info = usageInfo header os
  in do case eopts of
          Left err -> die info err
          Right opts -> do
            ecfg <- (runErrorT (processDBOptions opts))
            case ecfg of
              Left err -> die info err
              Right cfg -> do outConfig cfg opts
                              return cfg

