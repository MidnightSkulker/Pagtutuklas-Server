{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS -XFunctionalDependencies
            -XUndecidableInstances
            -XTypeSynonymInstances #-}
module AppActions
    ( proto
    , getCookies
    , getRemoteAddrStr
    ) where

import Prelude hiding (catch)

import qualified Snap.Types as S
import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromMaybe )
import State.RPXState ( HasRPXState, MonadRPX (..) )
import Application ( Application )

-- | Get the protocol used by the requests
proto :: (HasRPXState st) => Application st B.ByteString
proto = do -- Get the HTTP request from the Snap plumbing
           req <- S.getRequest
           -- Get the desired header from the HTTP requests
           let mhdr = S.getHeader "X-Forwarded-Proto" req
           -- Fish the default protocol out of the application state
           defProto <- fishDefaultProto
           -- Return either the default protocol or the protocol from the request
           return $ fromMaybe (B.pack defProto) mhdr

-- | Get the remote address string from the HTTP request
getRemoteAddrStr :: Application st (Maybe B.ByteString)
getRemoteAddrStr = do -- Get the HTTP request from the Snap plumbing
                      req <- S.getRequest
                      -- Get the desired header from the HTTP requests
                      let mhdr = S.getHeader "X-Real-IP" req
                      case mhdr of
                        Nothing -> return $ Just ( S.rqRemoteAddr req )
                        -- If address is found, return it
                        Just ip -> return $ Just ip

-- | Get the cookies from the HTTP request
getCookies :: Application st [(S.Cookie)]
getCookies = do req <- S.getRequest
                return ( S.rqCookies req )


-- | Prepare an "ok" response in the Snap internal state
-- okResp :: Bool -> String -> Application st ()
-- okResp a b = ok "text/javascript" ( body a b )

-- | Prepare the body of a response
-- body :: Bool -> String -> B.ByteString
-- body a b = B.pack ( "RPXNOW._base_cb(" ++ show a ++ ", " ++ b ++ ");" )

-- cfgRequestedUri :: String -> String -> Request LBS.ByteString -> URI
-- cfgRequestedUri host protocol req = do
--   let h = fromMaybe host (findHeader HdrHost req)
--       p = fromMaybe protocol (findHeader (HdrCustom "X-Forwarded-Proto") req)
--       invalid = error $ "Invalid URI build from request: " ++ show h
--       baseUrl = fromMaybe invalid $ parseURI $ p ++ "://" ++ h ++ "/"
--   fromJust $ rqURI req `relativeTo` baseUrl

-- -- | Return an absolute URI for the current request
-- requestedURI :: (AppState st c, HasMinState st, HasRPXState st) => AppM st URI
-- requestedURI = do { req  <- asksRequest id
--                   ; host <- asksAppState getRPXHost
--                   ; protocol <- asksAppState getDefaultProto
--                   ; return ( cfgRequestedUri host protocol req )
--                   }

-- -- | Build a URL relative to the current request's URL, given a
-- --   relative reference as a String.
-- --
-- -- XXX: this can fail if the relative reference is not a valid URI
-- relativeUrl :: (AppState st c, HasMinState st, HasRPXState st) =>
--                String -> AppM st URI
-- relativeUrl rel =
--   let invalid = error $ "Not a valid relative reference: " ++ show rel
--       r = fromMaybe invalid $ parseRelativeReference rel
--   in do rURI <- requestedURI
--         let mURI = relativeTo r rURI
--         return ( fromJust mURI )
