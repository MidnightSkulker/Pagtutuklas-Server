{-# LANGUAGE FlexibleContexts #-}
module Fetcher.Curl ( fetch ) where

import qualified Network.HTTP as HTTP
import Control.Arrow ( (>>>) )
import Fetcher.Common
import Fetcher.Errors ( FetchError, nameResolutionError, networkError, sslError )
import qualified Fetcher.Response as Resp
import qualified Fetcher.Request as Req
import Network.Curl
import Network.URI
import Control.Monad ( unless )
import Control.Monad.Error ( MonadError )
import Control.Monad.Trans ( liftIO, MonadIO )

curlFetch :: Req.Request -> IO CurlResponse
curlFetch req = do
  h <- initialize

  -- Fail if we get something other than CurlOK from setting an
  -- option, because the options we are setting should always succeed
  let setopt_ o = do
              r <- setopt h o
              unless (r == CurlOK) $
                     fail $ "Got " ++ show r ++ " when setting " ++ show o

  setopt_ $ CurlHttpHeaders $ httpHeadersToCurlHeaders (Req.headers req)
  setopt_ $ CurlURL $ show $ Req.uri req
  setopt_ $ CurlFailOnError False
  case Req.body req of
    Nothing -> return ()
    Just bdy -> do
              setopt_ $ CurlPost True
              setopt_ $ CurlPostFields [bdy]

  perform_with_response_ h

fetch :: (MonadIO m, MonadError e m, FetchError e) => FetcherM m
fetch req = liftIO (curlFetch req) >>= convertCurlResponse (Req.uri req)

convertCurlResponse :: (FetchError e, MonadError e m) =>
                       URI -> CurlResponse -> m Resp.Response
convertCurlResponse requestUri curlResp =
    case respCurlCode curlResp of
      CurlOK -> let headers = curlHeadersToHttpHeaders $ respHeaders curlResp
                in return Resp.Response { Resp.code = respStatus curlResp
                                        , Resp.headers = headers
                                        , Resp.body = respBody curlResp
                                        , Resp.uri = requestUri
                                        }
      CurlCouldntResolveHost ->
          nameResolutionError "Name resolution failed"
      CurlCouldntConnect ->
          networkError "Connecting to server failed"
      CurlSSLConnectError ->
          sslError "Failed to establish secure connection"
      CurlSSLShutdownFailed ->
          sslError "Failed to shut down secure connection"
      CurlSSLCertProblem ->
          sslError "Problem with secure server certificate"
      -- throw an Internal error since we don't expect other errors
      err -> fail $ show err

httpHeadersToCurlHeaders :: [HTTP.Header] -> [String]
httpHeadersToCurlHeaders = map show

curlHeadersToHttpHeaders :: [(String, String)] -> [HTTP.Header]
curlHeadersToHttpHeaders = map mkHeader >>> HTTP.parseHeaders >>> fromRes
    where mkHeader (n, v) = n ++ ": " ++ v
          fromRes = either (show >>> error) id

-- curlRedirectCodes :: [Int]
-- curlRedirectCodes = map redirectCodeToInt redirectCodes
