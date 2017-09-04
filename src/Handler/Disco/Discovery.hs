{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -XScopedTypeVariables #-}
module Handler.Disco.Discovery ( discoverIdentifier ) where

import Control.Monad.Error ( Error(..), ErrorT, runErrorT )
import Control.Monad.Trace ( NoTraceT, runNoTraceT )

import qualified Text.JSON as JSON
import qualified Snap.Types as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Fetcher.Fetcher ( fetcher )
import Application ( Application )
import State.ProxyState ( HasProxyState )
import Handler.Disco.JSONSerialization () -- For JSON instances
import RelyingParty.Util ( errorResponse, ParameterError ( .. ), okResponse )
import Discover.Discover ( discoverAsClaimedId )
import Discover.Discover ( discoverWith )
import Network.OpenID.JanRain.Endpoint ( Endpoint )
import Fetcher.Errors ( OpenIDError(..), DiscoveryErrorType(..), FetchErrorType(..) )
import Fetcher.Memoize ( runMemoizeFetcherT, MemoizeFetcherT )
import Fetcher.Common ( FetcherM )
import Network.OpenID.JanRain.Identifier ( Identifier )

{-
  THIS ACTION IS NEEDED BY /OPENID/FINISH IN RUBY FOR APPS FOR
  DOMAINS DISCOVERY
-}

discoverIdentifier :: ( HasProxyState st ) => Application st ()
discoverIdentifier = do
  -- Get the request parameters from the incoming HTTP request.
  -- S.Params = Map ByteString [ByteString]
  params :: S.Params <- S.withRequest (return . S.rqParams)
  -- Are we looking up the parameter correctly?????
  let mIdentArg :: Maybe [B.ByteString] = M.lookup "openid_identifier" params
  case mIdentArg of
    Nothing -> errorResponse (MissingParameter "openid_identifier") "text/plain" 400
    Just [identStr] -> do
      let identStr' = B.unpack identStr
      result <- discoverEndpoints discoverAsClaimedId fetcher identStr'
      case result of
         Right endpoints ->
           okResponse (B.pack (JSON.encode endpoints))
         Left err -> do
           S.logError ( B.pack ( "*** discovery failed for " ++ identStr' ) )
           case err of
             AppError msg -> fail msg
             UserVisibleError oide -> do
               (logMsg, userMsg) <- errorInfo oide
               S.logError ( B.pack ( "*** Additional information: identifier=" ++
                                     identStr' ++ " errorInfo=" ++ logMsg ) )
               errorResponse (InvalidParameter userMsg) "text/plain" 400
    Just _many -> do errorResponse (DatabaseError "More than one match") "text/plain" 400

data DiscoveryFailed = AppError String
                     | UserVisibleError OpenIDError

instance Error DiscoveryFailed where
  strMsg = AppError

type DiscoveryFunction m =
    Identifier -> MemoizeFetcherT OpenIDError (NoTraceT (ErrorT OpenIDError m)) [Endpoint]

discoverEndpoints :: Monad m => DiscoveryFunction m
                  -> FetcherM (NoTraceT (ErrorT OpenIDError m))
                  -> String -> m (Either DiscoveryFailed [Endpoint])
discoverEndpoints disco theFetcher ident = do
  result <- runErrorT $ runNoTraceT $
            runMemoizeFetcherT (discoverWith disco ident) theFetcher

  return $ case result of
             Left err -> Left $ UserVisibleError err

             Right [] -> error $ "Unexpectedly got no endpoints when discovering "
                         ++ show ident

             Right eps -> Right eps

-- |Returns information about the first failure that was seen
errorInfo :: Monad m => OpenIDError -> m (String, String)
errorInfo (SimpleFailure msg) = fail msg
errorInfo (DiscoveryError eType desc) = do
  msg <- case eType of
           InvalidInput -> return "Your OpenID must be a URL"
           PageNotFound -> return "The OpenID you entered was not found"
           NotAnOpenID ->
               return "The URL you entered does not appear to be an OpenID"
           BadOpenIDMetadata ->
               return "Discovery failed for the OpenID you entered"
           InternalError -> fail desc
  return (desc, msg)
errorInfo (FetchError eType desc) =
    let msg = case eType of
                NameResolution -> "The URL you entered does not appear to be an OpenID"
                Network -> "Unable to contact your OpenID"
                Server -> "There was an error looking up your OpenID"
                Ssl -> "SSL verification failed for your OpenID"
    in return (desc, msg)
