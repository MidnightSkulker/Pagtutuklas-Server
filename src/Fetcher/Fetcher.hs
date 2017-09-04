{-# OPTIONS -XScopedTypeVariables -XFlexibleInstances -XUndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module provides a function to build a fetcher from an HTTP request
module Fetcher.Fetcher
    ( Fetcher
    , fetcher
    ) where

import Control.Monad.Trans ( lift, liftIO )
import Control.Monad.Error ( ErrorT, runErrorT, throwError )
import Control.Monad.Trace ( NoTraceT, runNoTraceT )
import Config.HostPort ( HostPort )
import State.ResolverState ( HasResolverState (..), MonadResolver (..) )
import State.ProxyState ( MonadProxy (..), HasProxyState (..) )
import Fetcher.Common ( FetcherM )
import Fetcher.Errors ( OpenIDError )
import Fetcher.HTTP ( fetch )
import Fetcher.Util ( followRedirects )
import Handler.Disco.MX ( CanResolve(..) )
import Handler.Disco.SquidFetcher ( squidFetcher )
import qualified ADNS.Resolver as Resolver ( resolveMX )
import Application ( Application )

----------------------------------------------------------------------
--
-- Fetcher Summary
--  fetcherFromConfig
--    Build a fetcher monad from configuration information:
--    The only configuration information needed is the HostPort
--
--  fetcher
--    Produce a fetcher from an HTTP request
--
----------------------------------------------------------------------

-- | Standard Fetcher type, including a handler monad
--   Here are the Monad layers
--     FetcherM
--     NoTraceT
--     ErrorT
--     AppMIO
--      BagelT
--       ReaderT
--        IO
type Fetcher st c = FetcherM (NoTraceT (ErrorT OpenIDError (Application st)))

-- | Build a fetcher monad from configuration information:
--   The only configuration information needed is the HostPort
fetcherFromConfig :: Maybe HostPort -> Fetcher st c
fetcherFromConfig mHostPort req =
    let noProxyResp = fetch req
        run = runErrorT . runNoTraceT
    in case mHostPort of
         Nothing -> noProxyResp
         Just hostPort -> do
           let squidFetch = followRedirects 10 (squidFetcher hostPort)
--                **** Must make sure this is handled in Snap
--                handleIOErrors (e::IOException) = do
--                  tell $ Log.sendMail "Error using Squid proxy"
--                  tell $ Log.normal
--                          $ printf "*** Exception occurred using HTTP proxy \
--                                   \%s: %s" (formatHostPort hostPort) (show e)
--                  run noProxyResp

           eitherResp <- lift $ lift $ run (squidFetch req)
           either throwError return eitherResp

-- | Produce a fetcher from an HTTP request
fetcher ::( HasProxyState st ) => Fetcher st c
fetcher req = do
  mHostPort <- lift $ lift $ fishProxyHostPort
  fetcherFromConfig mHostPort req

instance ( HasResolverState st ) => CanResolve (Application st) where
    resolveMX host = do resolver <- fishResolver
                        liftIO $ Resolver.resolveMX resolver host
