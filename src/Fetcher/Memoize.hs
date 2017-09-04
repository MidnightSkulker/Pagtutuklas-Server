{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving #-}
module Fetcher.Memoize
    ( MemoizeFetcherT(..)
    , Cache
    , runMemoizeFetcherT
    ) where

import Prelude hiding ( log )
import Control.Applicative ( (<$>) )
import Control.Monad.RWS ( RWST, evalRWST )
import Control.Monad.State.Class ( gets, modify )
import Control.Monad.Reader.Class ( ask )
import Control.Monad.Trans ( lift )
import Control.Monad.Error.Class ( MonadError(throwError) )

import qualified Data.Map as Map
import Fetcher.Common ( FetcherM )
import Fetcher.Monad ( MonadFetch(..) )
import Fetcher.Response ( Response )
import Fetcher.Errors ( FetchError )
import Control.Monad.Trace ( TraceString, log, MonadTrace, MonadTracedErrors(..) )
import Control.Monad ( MonadPlus, liftM )
import Control.Monad.Trans ( MonadIO )

type Cache e = Map.Map String (Either e Response)

newtype MemoizeFetcherT e m a = MFT (RWST (FetcherM m) () (Cache e) m a)
    deriving ( Monad
             , MonadPlus
             , MonadError e
             , MonadTrace msg
             , MonadTracedErrors msg e
             , MonadIO
             , Functor
             )

runMemoizeFetcherT :: Monad m => MemoizeFetcherT e m a -> FetcherM m -> m a
runMemoizeFetcherT (MFT m) fetcher = liftM fst $ evalRWST m fetcher Map.empty

instance ( Functor m, MonadError e m, MonadTrace msg m, MonadTracedErrors msg e m
         , FetchError e, TraceString msg ) =>
    MonadFetch msg e (MemoizeFetcherT e m) where
        performHttp req =
            MFT $ do
              let cacheKey = show req
              cachedResult <- gets $ Map.lookup cacheKey
              case cachedResult of
                Just res -> do
                    log "Using memoized response"
                    either throwError return res

                Nothing -> do
                    f <- ask
                    res <- (Right <$> lift (f req))
                               `quietCatchError` (return . Left)
                    modify $ Map.insert cacheKey res
                    either quietThrowError return res
