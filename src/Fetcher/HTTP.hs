{-# LANGUAGE FlexibleContexts #-}
-- |OpenID library fetcher implementation using Network.HTTP
module Fetcher.HTTP
    ( fetch
    , fetchProxy
    , mkFetcher
    , proxyHTTP
    , PerformHTTP
    , Proxy
    , toHTTPRequest
    ) where

import Control.Applicative ( (<$>) )
import Control.Arrow ( (>>>) )
import Control.Monad ( when, join )
import Control.Monad.Trans ( liftIO, MonadIO )
import Data.List ( isPrefixOf )
import Network.TCP ( openStream )
import Network.URI ( URI, uriAuthority, uriUserInfo )
import System.IO.Error ( ioeGetLocation, isUserError, ioeGetErrorString )

import Network.HTTP
    ( Request(..)
    , Response(..)
    , RequestMethod(GET, POST)
    , ResponseCode
    , HeaderName(HdrContentLength, HdrContentType)
    , Header(..)
    , simpleHTTP
    , findHeader
    )
import Network.HTTP.HandleStream ( sendHTTP )
import Network.HTTP.Base
    ( normalizeRequest
    , NormalizeRequestOptions(..)
    , defaultNormalizeRequestOptions
    )
import Network.Stream ( Result, ConnError(..) )

import Fetcher.Errors ( FetchError, networkError, serverError, nameResolutionError )
import Control.Monad.Error ( MonadError )
import qualified Fetcher.Common as C
import qualified Fetcher.Request as FReq
import qualified Fetcher.Response as FResp

type Proxy = (String, Int)
type PerformHTTP = Request String -> IO (Result (Response String))

hasUserInfo :: Request String -> Bool
hasUserInfo = rqURI >>> uriAuthority >>> maybe False (not . null . uriUserInfo)

fetch :: (FetchError e, MonadIO m, MonadError e m) => C.FetcherM m
fetch = mkFetcher $ \req -> do
          when (hasUserInfo req) $
               fail $ "Fetching URIs with userinfo not supported" ++ show (rqURI req)
          simpleHTTP req


fetchProxy :: (FetchError e, MonadIO m, MonadError e m) => Proxy -> C.FetcherM m
fetchProxy p req = do
  httpResp <- liftIO $ proxyHTTP p $ toHTTPRequest req
  convertResp (FReq.uri req) httpResp

mkFetcher :: (FetchError e, MonadIO m, MonadError e m) => PerformHTTP -> C.FetcherM m
mkFetcher performHTTP req = do
  let uri = FReq.uri req
      httpReq = toHTTPRequest req

  httpResp <- convertExceptions $ performHTTP httpReq
  convertResp uri httpResp

-- | Normalize the request per the openID protocol
proxyNormalizeRequest :: Request ty -> Request ty
proxyNormalizeRequest =
    normalizeRequest $ defaultNormalizeRequestOptions { normForProxy = True }

-- | Forward a normalized request to a proxy.
proxyHTTP :: Proxy -> PerformHTTP
proxyHTTP (host, port) req = do
  -- Open a TCP connection, resulting in a @stream@
  c <- openStream host port
  -- Normalize the request and send on the newly opened stream
  -- This does not close the stream.
  sendHTTP c $ proxyNormalizeRequest req

toHTTPRequest :: FReq.Request -> Request String
toHTTPRequest req =
    case FReq.body req of
      Nothing -> Request { rqMethod = GET
                         , rqBody = ""
                         , rqURI = FReq.uri req
                         , rqHeaders = FReq.headers req
                         }
      Just body ->
          let extraHeaders = concatMap defaultHeader headerDefaults
              defaultHeader (h, dflt) = maybe [Header h dflt] (const []) $
                                        findHeader h req
              headerDefaults =
                  [ (HdrContentLength, show $ length body)
                  , (HdrContentType, "application/x-www-form-urlencoded")
                  ]
          in Request { rqMethod = POST
                     , rqBody = body
                     , rqURI = FReq.uri req
                     , rqHeaders = extraHeaders ++ FReq.headers req
                     }

mkResp :: URI -> Response String -> FResp.Response
mkResp uri httpResp =
    FResp.Response { FResp.uri = uri
                   , FResp.headers = rspHeaders httpResp
                   , FResp.body = rspBody httpResp
                   , FResp.code = redirectCodeToInt $ rspCode httpResp
                   }

convertResp :: (MonadError e m, FetchError e) =>
               URI -> Result (Response String) -> m FResp.Response
convertResp uri = either convertConnErrors (return . mkResp uri)

convertConnErrors :: (MonadError e m, FetchError e) => ConnError -> m a
convertConnErrors ErrorReset = networkError "Connection reset"
convertConnErrors ErrorClosed = networkError "Connection closed"
convertConnErrors (ErrorParse s) =
    serverError $ "Error parsing server response: " ++ s
convertConnErrors (ErrorMisc s) = networkError $ "Network error: " ++ s

convertExceptions :: (FetchError e, MonadError e m, MonadIO m) => IO a -> m a
convertExceptions act =
  let result = catch (return <$> act) $ \e ->
               case ioeGetLocation e of
                 "getHostByName" ->
                     return $ nameResolutionError $ show e
                 "connect" -> return $ networkError $ show e
                 _ | isHostLookupError e -> return $ nameResolutionError $ show e
                 _ -> ioError e

      -- The HTTP library is kind of gross this way. This may break if
      -- the implementation of Network.TCP.openTCPConnection (in the
      -- HTTP package) changes.
      isHostLookupError e =
          let errStr = ioeGetErrorString e
          in isUserError e && (noAddrs `isPrefixOf` errStr ||
                               lookupFailure `isPrefixOf` errStr)
      noAddrs = "openTCPConnection: no addresses in host entry for "
      lookupFailure = "openTCPConnection: host lookup failure for "

  in join $ liftIO result

redirectCodeToInt :: ResponseCode -> Int
redirectCodeToInt (a, b, c) = a * 100 + b * 10 + c
