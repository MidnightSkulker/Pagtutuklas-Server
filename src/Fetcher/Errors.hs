{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Fetcher.Errors
    ( DiscoveryError(..)
    , FetchError(..)
    , DiscoveryErrorType(..)
    , FetchErrorType(..)
    , OpenIDError(..)
    , invalidInput
    , nameResolutionError
    , networkError
    , pageNotFound
    , serverError
    , notAnOpenID
    , badOpenIDMetadata
    , sslError
    , internalError

    , catchFailure

    , firstSucceeding
    ) where

import Control.Monad.Trace ( MonadTracedErrors(..), logMessage, MonadTrace, TraceString, EmbedsMPlus(..) )
import Control.Monad.Error ( Error(..), MonadError(..) )
import Data.Monoid ( Monoid(..) )

class (Monoid e, Error e) => DiscoveryError e where
    embedDiscoveryError :: DiscoveryErrorType -> String -> e
    getDiscoveryErrorType :: e -> Maybe DiscoveryErrorType
    getFailure :: e -> Maybe String

catchFailure :: (MonadError e m, DiscoveryError e, MonadTracedErrors msg e m) =>
                m a -> (String -> m a) -> m a
act `catchFailure` handler =
    act `quietCatchError` \e ->
        case getFailure e of
          Just errorString -> handler errorString
          Nothing -> quietThrowError e

data OpenIDError = DiscoveryError DiscoveryErrorType String
                 | FetchError FetchErrorType String
                 | SimpleFailure String
                   deriving Show

instance Monoid OpenIDError where
    mempty = SimpleFailure "mempty"
    (SimpleFailure _) `mappend` e2 = e2
    _ `mappend` e2@(DiscoveryError InternalError _) = e2
    e1 `mappend` _ = e1

-- |This is like msum, except it uses the Monoid instance of the error
-- type to aggregate the errors as desired
firstSucceeding :: (Monad m, MonadTracedErrors msg e m, DiscoveryError e,
                    Monoid e, MonadTrace msg m, TraceString msg,
                    EmbedsMPlus msg) =>
                   [m a] -> m a
firstSucceeding [] = do
  logMessage embedMzero
  fail "No alternatives succeeded"

firstSucceeding (act:acts) =
    act `quietCatchError` \e1 ->
        case getDiscoveryErrorType e1 of
          -- InternalError short-circuts
          Just InternalError -> quietThrowError e1
          _ -> do
            logMessage embedMplusFailure
            firstSucceeding acts `quietCatchError` \e2 ->
                quietThrowError $ e1 `mappend` e2

instance Error OpenIDError where
    strMsg = SimpleFailure

instance DiscoveryError OpenIDError where
    embedDiscoveryError = DiscoveryError
    getFailure (SimpleFailure msg) = Just msg
    getFailure _ = Nothing
    getDiscoveryErrorType (DiscoveryError typ _) = Just typ
    getDiscoveryErrorType _ = Nothing

instance FetchError OpenIDError where
    embedFetchError = FetchError

-- |Possible types of discovery errors
data DiscoveryErrorType
    = InvalidInput
    | NotAnOpenID
    | BadOpenIDMetadata
    | PageNotFound
    | InternalError
      deriving (Eq, Show)

class Error e => FetchError e where
    embedFetchError :: FetchErrorType -> String -> e

-- |Possible types of fetching errors
data FetchErrorType
    = NameResolution
    | Network
    | Server
    | Ssl
      deriving (Eq, Show)

invalidInput, pageNotFound, notAnOpenID, badOpenIDMetadata, internalError ::
    (DiscoveryError e, MonadError e m) => String -> m a
invalidInput = tde InvalidInput
notAnOpenID = tde NotAnOpenID
badOpenIDMetadata = tde BadOpenIDMetadata
pageNotFound = tde PageNotFound
internalError = tde InternalError

tde :: (DiscoveryError e, MonadError e m) =>
       DiscoveryErrorType -> String -> m a
tde t = throwError . embedDiscoveryError t

nameResolutionError, networkError, serverError, sslError ::
    (FetchError e, MonadError e m) => String -> m a
nameResolutionError = throwError . embedFetchError NameResolution
networkError = throwError . embedFetchError Network
serverError = throwError . embedFetchError Server
sslError = throwError . embedFetchError Ssl
