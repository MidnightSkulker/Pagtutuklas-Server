-- | HostPort definition
module Config.HostPort
       ( HostPort (..)
       , formatHostPort
       ) where

import Network.BSD ( HostEntry, hostName )

-- | Information about a host port
data HostPort =
  HostPort { hostEntry :: HostEntry  -- ^ Translated IP v4 address
           , hostPort  :: Int        -- ^ TCP Port number
           , hostAddr  :: String     -- ^ Address saved for future reference
           }

instance Show HostPort where
  show h = hostName (hostEntry h) ++ ":" ++ show (hostPort h) ++ " ( " ++
           hostAddr h ++ ")"

-- | Prepare a host address of the form a.b.c.d:p
formatHostPort :: HostPort -> String
formatHostPort h = hostName (hostEntry h) ++ ":" ++ show (hostPort h)
