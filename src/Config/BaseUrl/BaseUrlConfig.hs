{-# OPTIONS -XFlexibleInstances -XUndecidableInstances #-}
-- | Collect all the configuration information for the BaseUrl server
module Config.BaseUrl.BaseUrlConfig ( BaseUrlConfig (..) ) where

import Config.DBConfig ( DBConfig ( .. ) , HasDBConfig (..) )
import Config.RPXConfig ( RPXConfig ( .. ), HasRPXConfig (..) )
import Config.LogConfig ( LogConfig (..), HasLogConfig (..) )
import Config.MailConfig ( MailConfig ( .. ), HasMailConfig (..) )
import Config.MinConfig ( MinConfig ( .. ), HasMinConfig (..) )
import Config.CacheConfig ( CacheConfig ( .. ), HasCacheConfig (..) )

-- | All the configuration information needed for the BaseUrl server.
data BaseUrlConfig = BaseUrlConfig
      { dbConfig        :: DBConfig      -- ^ Database connection information
      , rpxConfig       :: RPXConfig     -- ^ General RPX server information
      , minConfig       :: MinConfig     -- ^ Minimum configuration for a server
      , mailConfig      :: MailConfig    -- ^ Mail configuration, for e-mailing developers
      , cacheConfig     :: CacheConfig   -- ^ Config relevant to DB cache
      , logConfig       :: LogConfig     -- ^ Configuration for the server log
      }

instance HasMinConfig BaseUrlConfig where
  getMinConfig = minConfig

instance HasCacheConfig BaseUrlConfig where
  getCacheConfig = cacheConfig

instance HasLogConfig BaseUrlConfig where
  getLogConfig = logConfig

instance HasMailConfig BaseUrlConfig where
  getMailConfig = mailConfig

instance HasDBConfig BaseUrlConfig where
  getDBConfig = dbConfig

instance HasRPXConfig BaseUrlConfig where
  getRPXConfig = rpxConfig

instance Show BaseUrlConfig where
  show BaseUrlConfig { dbConfig = db, rpxConfig = rpx, minConfig = mi
                     , mailConfig = mail, logConfig = lg, cacheConfig = c } =
    "DBConfig: " ++ show db ++ "\n" ++
    "RPXConfig: " ++ show rpx ++ "\n" ++
    "MinConfig: " ++ show mi ++ "\n" ++
    "MailConfig: " ++ show mail ++ "\n" ++
    "CacheConfig: " ++ show c ++ "\n" ++
    "LogConfig: " ++ show lg ++ "\n"
