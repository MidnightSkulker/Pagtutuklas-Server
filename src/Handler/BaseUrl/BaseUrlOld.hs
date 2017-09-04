{-# LANGUAGE OverloadedStrings #-}
module Handler.JsApi ( baseUrl ) where

import Prelude hiding ( lookup )

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Text.JSON as JSON

import Network.URI ( URI )
import Network.HTTP.Headers ( HeaderName(HdrCacheControl) )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Control.Monad.Trans ( lift, liftIO )
import Data.Cache.LRU.IO ( AtomicLRU, insert, lookup )
-- Janrain
import qualified RelyingParty.Util as Util
import qualified RelyingParty.RelyingParty as RP
import State.BaseUrlState ( BaseUrlCacheKey, getBaseUrlCacheKey, HasBaseUrlState )
import State.DBState      ( HasDBState )
import State.RPXState    ( HasRPXState (..) )
import Bagel.HasRequest  ( askQueryArg )
import Bagel.Response    ( LazyResponse, ok, withContentType, addHeader )

      
fromDB :: (AppState st c, HasDBState st, HasRPXState st) =>
          BaseUrlCacheKey -> AtomicLRU BaseUrlCacheKey URI ->
          ExceptionalT Util.ParameterError (AppMIO st) URI
fromDB pair@(_, mXdUrl) cache = do
  rp <- Util.getRpByAppId
  case mXdUrl of
    Nothing -> return ()
    Just xdUrl -> do
      valid <- lift . RP.tokenUrlAllowed rp . Util.urlToURI $ xdUrl
      when (not valid) . throwT . Util.InvalidParameter $
               "xdReceiver is not a whitelisted url"
  uri <- lift $ RP.rpBaseUri rp
  lift . liftIO $ insert pair uri cache
  return uri
      
baseUrl' :: (AppState st c, HasBaseUrlState st, HasDBState st, HasRPXState st) =>
            ExceptionalT Util.ParameterError (AppMIO st) LazyResponse
baseUrl' = do
  pair <- do
    appId <- Util.getRequiredParam "appId"
    skip <- lift $ askQueryArg "skipXdReceiver"
    case skip of
      Nothing -> (,) appId . Just <$> Util.getHTTPsURLParam "xdReceiver"
      Just _ -> return (appId, Nothing)

  cache <- lift $ asksAppState getBaseUrlCacheKey
  mUri <- lift . liftIO $ lookup pair cache
  uri <- maybe (fromDB pair cache) return mUri

  return $ resp True (show uri) `withMaxAge` "307584000"
      
withMaxAge :: LazyResponse -> String -> LazyResponse
withMaxAge r age = addHeader HdrCacheControl maxAge r
  where maxAge = "max-age=" ++ age
      
resp :: Bool -> String -> LazyResponse
resp a b = (ok $ body a b) `withContentType` "text/javascript"
      
body :: Bool -> String -> LBS.ByteString
body a b = LBS.concat [ "RPXNOW._base_cb("
                      , UTF8.fromString $ JSON.encode a
                      , ", "
                      , UTF8.fromString $ JSON.encode b
                      , ");"
                      ]

baseUrl :: (AppState st c, HasRPXState st, HasDBState st, HasBaseUrlState st) =>
           AppHandler st
baseUrl = resolveT handler baseUrl'
    where
      handler :: Util.ParameterError -> AppHandler st
      handler (Util.MissingParameter s) =
          return . resp False $ "Missing Parameter: " ++ s
      handler (Util.InvalidParameter s) =
          return . resp False $ "Invalid Parameter: " ++ s
