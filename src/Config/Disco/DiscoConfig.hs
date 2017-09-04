{-# OPTIONS -XFlexibleInstances -XUndecidableInstances #-}
-- | Collect all the configuration information for the Disco server
module Config.Disco.DiscoConfig ( DiscoConfig (..) ) where

import Config.RPXConfig   ( RPXConfig ( .. ), HasRPXConfig (..) )
import Config.LogConfig   ( LogConfig (..), HasLogConfig (..) )
import Config.MailConfig  ( MailConfig ( .. ), HasMailConfig (..) )
import Config.MinConfig   ( MinConfig ( .. ), HasMinConfig (..) )
import Config.ProxyConfig ( ProxyConfig ( .. ), HasProxyConfig (..) )

-- | All the configuration information needed for the Disco server.
data DiscoConfig = DiscoConfig
      { rpxConfig       :: RPXConfig     -- ^ General RPX server information
      , minConfig       :: MinConfig     -- ^ Minimum configuration for a server
      , mailConfig      :: MailConfig    -- ^ Mail configuration, for e-mailing developers
      , logConfig       :: LogConfig     -- ^ Configuration for the server log
      , proxyConfig     :: ProxyConfig   -- ^ Configuration of the proxy server
      }

instance HasMinConfig DiscoConfig where
  getMinConfig = minConfig

instance HasLogConfig DiscoConfig where
  getLogConfig = logConfig

instance HasMailConfig DiscoConfig where
  getMailConfig = mailConfig

instance HasRPXConfig DiscoConfig where
  getRPXConfig = rpxConfig

instance HasProxyConfig DiscoConfig where
  getProxyConfig = proxyConfig

instance Show DiscoConfig where
  show DiscoConfig { rpxConfig = rpx, minConfig = mi, proxyConfig = p
                   , mailConfig = mail, logConfig = lg } =
    "RPXConfig: " ++ show rpx ++ "\n" ++ "MinConfig: " ++ show mi ++ "\n" ++
    "MailConfig: " ++ show mail ++ "\n" ++ "LogConfig: " ++ show lg ++ "\n" ++
    "ProxyConfig: " ++ show p ++ "\n"
