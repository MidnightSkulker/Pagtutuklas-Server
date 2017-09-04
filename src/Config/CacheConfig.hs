-- | Configuration information that is relevant to the cache
module Config.CacheConfig
       ( CacheConfig (..)
       , HasCacheConfig (..)
       , processCacheOptions  
       , cacheConfigOptions
       ) where

-- Haskell
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO )
import System.Console.GetOpt
-- Janrain
import Config.ConfigClass ( Config (..), outConfig )
import Config.Options ( Option (..), argsToOptions, die )

-- | Options relevant to Cache configuration
cacheConfigOptions :: [ OptDescr Option ]
cacheConfigOptions = []

-- | Configuration information relevant to the cache function
data CacheConfig = CacheConfig deriving ( Show )

-- | Class characterizing having CacheConfig
class HasCacheConfig c where
  getCacheConfig :: c -> CacheConfig

instance HasCacheConfig CacheConfig where
  getCacheConfig = id

instance Config CacheConfig where
  parseConfig = parseCacheConfig
  options = const cacheConfigOptions

-- | Parse the beanstalk configuration data
parseCacheConfig :: String -> -- ^ Version string
                    String -> -- ^ Build String
                    String -> -- ^ Program name
                    [String] -> -- ^ Command ling args
                    [OptDescr Option] -> -- ^ Relevant options
                    IO CacheConfig -- ^ Beanstalk config
parseCacheConfig versionString buildString progName args os =
  let header = versionString ++ "\nUsage: " ++ progName
      eopts  = argsToOptions versionString buildString progName args os
      info   = usageInfo header os
  in case eopts of
          Left err -> die info err
          Right opts -> do
            ecfg <- (runErrorT (processCacheOptions opts))
            case ecfg of
               Left err -> die info err
               Right cfg -> do outConfig cfg opts
                               return cfg

-- | Parsing of Cache config
processCacheOptions :: (MonadIO m, MonadError e m) =>
                         [Option] -> m CacheConfig
processCacheOptions _opts = return ( CacheConfig )


