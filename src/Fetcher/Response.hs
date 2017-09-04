module Fetcher.Response ( Response(..) ) where

import Network.URI ( URI )
import Network.HTTP.Headers ( Header, HasHeaders(..) )

data Response =
    Response { uri :: URI
             , headers :: [Header]
             , body :: String
             , code :: Int
             } deriving Show

instance HasHeaders Response where
    getHeaders = headers
    setHeaders resp hdrs = resp { headers = hdrs }
