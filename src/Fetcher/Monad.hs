{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances,
             GeneralizedNewtypeDeriving #-}
module Fetcher.Monad
       ( Fetchable (..)
       , MonadFetch (..)
       , FetcherT (..)
       , runFetcherT
       ) where

import Control.Monad.Error ( MonadError )
import Network.URI ( URI )
import qualified Fetcher.Request as FReq
import qualified Fetcher.Response as FResp
import Fetcher.Errors ( FetchError )
import Fetcher.Common
import qualified Network.HTTP as HTTP
import Control.Monad ( MonadPlus )
import Control.Monad.Trace ( MonadTrace, TraceString, MonadTracedErrors, frame )
import Control.Monad.Trans ( MonadIO, lift )
import Control.Monad.Reader ( ReaderT, runReaderT, ask )

class Fetchable a where
    reqUri :: MonadError e m => a -> m URI
    reqHeaders :: MonadError e m => a -> m [HTTP.Header]
    reqHeaders = const (return [])
    reqBody :: MonadError e m => a -> m (Maybe String)
    reqBody = const (return Nothing)
    mkReq :: MonadError e m => a -> m FReq.Request
    mkReq x = do u <- reqUri x
                 h <- reqHeaders x
                 b <- reqBody x
                 return FReq.Request { FReq.uri     = u
                                     , FReq.headers = h
                                     , FReq.body    = b
                                     }

class ( MonadTrace msg m, TraceString msg, MonadError e m, FetchError e ) =>
    MonadFetch msg e m where
    performHttp :: FetcherM m
    httpGet :: Fetchable a => a -> m FResp.Response
    httpGet a = do req <- mkReq a
                   frame ("Fetching " ++ show (FReq.uri req)) ( performHttp req )
    httpGetContents :: Fetchable a => a -> m String
    httpGetContents a = do resp <- httpGet a
                           return ( FResp.body resp )

-- Might want to keep this one
-- instance Fetchable FReq.Request where
--     reqUri = return . FReq.uri
--     reqHeaders = return . FReq.headers
--     reqBody = return . FReq.body
--     mkReq = return

-- instance Fetchable URI where
--   reqUri = return

instance Fetchable (URI, [HTTP.Header]) where
  reqUri = return . fst
  reqHeaders = return . snd

-- instance Fetchable String where
--     reqUri s = case parseURI s of
--                  Nothing -> fail $ "Not a URI: " ++ show s
--                  Just u -> return u

newtype FetcherT m a = FetcherT { unFetcherT :: ReaderT (FetcherM m) m a }
    deriving ( MonadTrace msg
             , MonadError e
             , Monad
             , MonadPlus
             , MonadTracedErrors msg e
             , MonadIO
             , Functor
             )

runFetcherT :: ( FetchError e, MonadPlus m, MonadTrace msg m, TraceString msg
               , MonadError e m ) => FetcherT m a -> FetcherM m -> m a
runFetcherT act f = runReaderT (unFetcherT act) f

instance ( FetchError e, MonadPlus m, MonadTrace msg m, TraceString msg
         , MonadError e m ) =>
    MonadFetch msg e (FetcherT m) where
        performHttp req = do f <- FetcherT ask
                             FetcherT $ lift $ f req
