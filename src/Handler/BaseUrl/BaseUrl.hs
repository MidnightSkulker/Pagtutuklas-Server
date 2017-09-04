{-# LANGUAGE OverloadedStrings #-}
module Handler.BaseUrl.BaseUrl ( baseUrl ) where

import Prelude hiding ( lookup )

-- Haskell and hackage
import qualified Data.ByteString.Char8 as B
import qualified Snap.Types            as S
import qualified Data.Map              as M
import Data.Cache.LRU.IO ( AtomicLRU, insert, lookup )
import Data.CIByteString ( toCI )
import Network.URI ( URI )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
-- Janrain
import RelyingParty.RelyingParty ( tokenUrlAllowed, rpBaseUri )
import RelyingParty.Util ( ParameterError (..), getHTTPsURLParam, urlToURI 
                         , getRpByAppId, getRequiredParam, errorResponse 
                         , okResponse )
import State.CacheState ( CacheKey )
import State.BaseUrl.BaseUrlState ( fishCacheKey, HasBaseUrlState )
import BaseUrl.Application ( Application )
import State.RPXState ( HasRPXState )

-- | Get the relying party information from the RP database.
fromDB :: (HasRPXState st) => CacheKey -> AtomicLRU CacheKey URI -> Application st URI
fromDB pair@(_, mXdUrl) cache = do
  rp <- getRpByAppId
  case mXdUrl of
    Nothing -> return ()
    Just xdUrl -> do
      valid <- tokenUrlAllowed rp . urlToURI $ xdUrl
      when (not valid) $ errorResponse (InvalidParameter "xdReceiver is not a whitelisted url") "text/plain" 400
  uri <- rpBaseUri rp
  liftIO $ insert pair uri cache
  return uri
      
baseUrl :: ( HasBaseUrlState st, HasRPXState st ) => Application st ( )
baseUrl = do
  pair <- do
    -- Get the application ID from the incoming HTTP request.
    appId <- getRequiredParam (B.pack "appId")
    -- Get the request parameters from the incoming HTTP request.
    params <- S.withRequest (return . S.rqParams)
    -- Are we looking up the parameter correctly?????
    let mres = M.lookup "skipXdReceiver" params
    case mres of
      Nothing -> do url <- getHTTPsURLParam "xdReceiver"
                    return ( appId, Just url)
      Just _  -> return (appId, Nothing)
  -- Fish the cache out of the application state
  cache <- fishCacheKey
  -- First look in the cache for the application id
  mUri <- liftIO $ lookup pair cache
  -- If it was in the cache, just return it, otherwise hit the database
  uri <- maybe (fromDB pair cache) return mUri
  -- Generate a response with the good news
  okResponse (B.pack (show uri))
  -- Add some information about maximum age in the case (weird)
  withMaxAge "307584000"

-- | Add the maximum age of the cache entry to the response
withMaxAge :: String -> Application st ()
withMaxAge age =
  S.modifyResponse (S.addHeader (toCI (B.pack "HdrCacheControl"))
                                (B.pack ("max-age=" ++ age)))

-- baseUrl :: Application ()
-- baseUrl = resolveT handler baseUrl'
--     where
--       handler :: Util.ParameterError -> AppHandler st
--       handler (Util.MissingParameter s) =
--           return . resp False $ "Missing Parameter: " ++ s
--       handler (Util.InvalidParameter s) =
--           return . resp False $ "Invalid Parameter: " ++ s
