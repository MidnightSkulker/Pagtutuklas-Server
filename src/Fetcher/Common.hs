module Fetcher.Common ( FetcherM ) where

import Fetcher.Response ( Response )
import Fetcher.Request ( Request )

-- |Make a HTTP request and return a HTTP result
type FetcherM m = Request -> m Response
