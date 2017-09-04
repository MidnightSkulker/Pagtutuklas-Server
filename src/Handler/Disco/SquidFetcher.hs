{-# LANGUAGE FlexibleContexts #-}
module Handler.Disco.SquidFetcher ( squidFetcher ) where

import Data.Maybe ( fromMaybe )
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Error ( MonadError )

import Network.HTTP.Headers ( HeaderName(HdrCustom), findHeader )
import Network.BSD ( hostName )

import Config.HostPort ( HostPort ( .. ) )

import qualified Fetcher.Response as Response
import Fetcher.Errors ( OpenIDError(..), sslError, networkError, nameResolutionError )
import Fetcher.Common ( FetcherM )
import Fetcher.HTTP ( fetchProxy )

squidFetcher :: (MonadError OpenIDError m, MonadIO m) => HostPort -> FetcherM m
squidFetcher HostPort { hostEntry = hostEnt, hostPort = port, hostAddr = _addr } req = do
  resp <- fetchProxy (hostName hostEnt, fromEnum port) req

  fromMaybe (return ()) $ do
    squidError <- findHeader squidErrorHeader resp
    lookup ((Response.code resp), squidError) squidErrors

  return resp

squidErrorHeader :: HeaderName
squidErrorHeader = HdrCustom "X-Squid-Error"

squidErrors :: MonadError OpenIDError m => [((Int, String), m a)]
squidErrors =
    -- Invalid protocol (expecting SSL)
    -- Bad cert (not signed by recognized CA)
    [ ( ( 503, "ERR_CONNECT_FAIL 71" )
      , sslError "Either bad cert or SSL connection to non-SSL"
      )

    -- Access Denied
    -- Server up, but not responding to port
    , ( ( 503, "ERR_CONNECT_FAIL 111" )
      , networkError "Server did not respond"
      )

    -- DNS failure
    , ( ( 503, "ERR_DNS_FAIL 0" )
      , nameResolutionError "Squid DNS failure"
      )

    -- Server not up
    , ( ( 504, "ERR_CONNECT_FAIL 110" )
      , networkError "Server unreachable"
      )
    ]
