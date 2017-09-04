{-# OPTIONS -XFlexibleInstances -XUndecidableInstances #-}
-- | Collect all the configuration information for the Beanstalk server
module Config.RpxMe.RpxMeConfig ( RpxMeConfig (..) ) where

import Config.DBConfig ( DBConfig ( .. ) , HasDBConfig (..) )
import Config.RPXConfig ( RPXConfig ( .. ), HasRPXConfig (..) )
import Config.LogConfig ( LogConfig (..), HasLogConfig (..) )
import Config.MailConfig ( MailConfig ( .. ), HasMailConfig (..) )
import Config.MinConfig ( MinConfig ( .. ), HasMinConfig (..) )
import Config.BeanstalkConfig ( BeanstalkConfig ( .. ), HasBeanstalkConfig (..) )

-- | All the configuration information needed for the Beanstalk server.
data RpxMeConfig = RpxMeConfig
      { dbConfig        :: DBConfig      -- ^ Database connection information
      , rpxConfig       :: RPXConfig     -- ^ General RPX server information
      , minConfig       :: MinConfig     -- ^ Minimum configuration for a server
      , mailConfig      :: MailConfig    -- ^ Mail configuration, for e-mailing developers
      , beanstalkConfig :: BeanstalkConfig -- ^ Beanstalk config, for offline processing  
      , logConfig       :: LogConfig     -- ^ Configuration for the server log
      }

instance HasMinConfig RpxMeConfig where
  getMinConfig = minConfig

instance HasBeanstalkConfig RpxMeConfig where
  getBeanstalkConfig = beanstalkConfig

instance HasLogConfig RpxMeConfig where
  getLogConfig = logConfig

instance HasMailConfig RpxMeConfig where
  getMailConfig = mailConfig

instance HasDBConfig RpxMeConfig where
  getDBConfig = dbConfig

instance HasRPXConfig RpxMeConfig where
  getRPXConfig = rpxConfig

instance Show RpxMeConfig where
  show RpxMeConfig { beanstalkConfig = bean, dbConfig = db
                   , rpxConfig = rpx, minConfig = mi
                   , mailConfig = mail, logConfig = lg } =
    "DBConfig: " ++ show db ++ "\n" ++
    "BeanstalkConfig: " ++ show bean ++ "\n" ++
    "RPXConfig: " ++ show rpx ++ "\n" ++
    "MinConfig: " ++ show mi ++ "\n" ++
    "MailConfig: " ++ show mail ++ "\n" ++
    "LogConfig: " ++ show lg ++ "\n"
