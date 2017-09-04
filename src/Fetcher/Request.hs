module Fetcher.Request ( Request(..) ) where

import Network.URI ( URI )
import Network.HTTP.Headers ( Header, HasHeaders(..) )

data Request = Request { uri :: URI
                       , headers :: [Header]
                       , body :: Maybe String
                       } deriving Show

instance HasHeaders Request where
    getHeaders = headers
    setHeaders req hdrs = req { headers = hdrs }
