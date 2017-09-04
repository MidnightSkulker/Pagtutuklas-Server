{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- | A wrapper around making HTTP requests that is sufficient for doing
-- OpenID discovery. If this library grows beyond discovery, more work
-- will have to be done to make the fetcher support making POST
-- requests.
module Fetcher.Util
    ( FetcherM
    , fetch
    , fetchIO

    , FetcherT
    , runFetcherT

    , MemoizeFetcherT
    , runMemoizeFetcherT

    -- * MonadReader wrapper to make a fetcher available
    , MonadFetch(..)
    , Fetchable(..)

    -- * Fetcher Transformers
    , followRedirects
    , fetchByScheme
    , failUnless200
    ) where

import Data.Char ( toLower )
import Control.Arrow ( first )
import qualified Network.HTTP as HTTP
import Network.URI (parseURI, uriScheme, parseRelativeReference, relativeTo, URI )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Trans ( MonadIO )

import Fetcher.Errors
import Control.Monad.Trace ( MonadTrace, TraceString, frame, runNoTraceT )
import Fetcher.Common
import qualified Fetcher.Response as FResp
import qualified Fetcher.Request as FReq
import qualified Fetcher.Curl as Curl
import qualified Fetcher.HTTP as HTTPFetcher
import Fetcher.Monad ( FetcherT, runFetcherT, Fetchable (..), MonadFetch (..) )
import Fetcher.Memoize

failUnless200 :: ( DiscoveryError e, FetchError e, MonadTrace msg m,
                   TraceString msg, MonadError e m) =>
                 FetcherM m -> FetcherM m
failUnless200 fetcher req = do
  resp <- fetcher req
  let rcode = FResp.code resp
  case rcode of
    200 -> return resp
    404 -> pageNotFound "404 response"
    _ -> serverError $ "Unexpected response code: " ++ show rcode

redirectCodes :: [Int]
redirectCodes = [302, 301, 303, 307]

-- |A pretty sane default fetcher. Uses cURL if the URL is HTTP and
-- Network.HTTP otherwise. Will attempt to follow redirects, up to 10
-- times.
--
-- This fetcher does not have any special handling of SSL
-- certificates. It just relies on libcurl for that.
fetch :: ( MonadTrace msg m, TraceString msg, MonadError e m
         , MonadIO m, FetchError e) =>
           FetcherM m
fetch = followRedirects 10 $
        fetchByScheme [("http:", HTTPFetcher.fetch), ("https:", Curl.fetch)]

-- |A fetcher that runs in IO
fetchIO :: FetcherM IO
fetchIO req = either (fail . (show :: OpenIDError -> String)) return
            =<< runNoTraceT (runErrorT $ fetch req)

-- |Convert a fetcher that does one request into a fetcher that
-- follows redirects, and aborts after performing `tries` HTTP
-- requests (tries - 1 redirects)
followRedirects :: ( FetchError e, MonadTrace msg m, TraceString msg
                   , MonadError e m, Monad m ) =>
                       Int -> -- ^Allowed number of retries
                       FetcherM m -> -- ^ 
                       FetcherM m
followRedirects tries _ _ | tries < 1 = serverError "Too many redirects"
followRedirects tries fetcher req = do
  resp <- fetcher req
  if FResp.code resp `elem` redirectCodes
    then case HTTP.findHeader HTTP.HdrLocation resp >>= parseLocation of
           Nothing -> serverError "Invalid or missing location header"
           Just redirUri ->
               let req' = req { FReq.uri = redirUri }
               in frame ("Followed redirect to " ++ show redirUri ++ " ") $
                  followRedirects (tries - 1) fetcher req'
    else return resp

    where
      baseUri = FReq.uri req

      parseLocation loc =
          case parseURI loc of
            Nothing -> do path <- parseRelativeReference loc
                          relativeTo path baseUri
            result -> result

-- |Select a fetcher based on the scheme of the URI in the request.
fetchByScheme :: Monad m => [(String, FetcherM m)] -> FetcherM m
fetchByScheme fetchers req =
    let scheme = normalizeScheme $ uriScheme (FReq.uri req)
        fetcherNotFound = fail $ "Unsupported URI scheme: " ++ scheme
        normalizedFetchers = map (first normalizeScheme) fetchers
    in maybe fetcherNotFound ($ req) $ lookup scheme normalizedFetchers

-- |Given a URI scheme with or without a colon, return a normalized
-- URI scheme (lower-case, without the colon)
normalizeScheme :: String -> String
normalizeScheme schemeStr = map toLower $
                            case reverse schemeStr of
                              (':':scm) -> reverse scm
                              _ -> schemeStr
