{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Monad.Error.Log
    ( LogErrorT(..)
    )
where

import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Error ( MonadError(..), Error(..) )
import Control.Monad ( MonadPlus(..) )

-- data WrappedError e = WrappedError Int e

-- wrap :: e -> WrappedError e
-- wrap = WrappedError 0

-- level :: WrappedError e -> Int
-- level (WrappedError l _) = l

-- instance Error e => WrappedError e where
--     strMsg = wrap . strMsg

data LogErrorT e m a = LogErrorT { runLogErrorT :: m (Either e a) }

instance (MonadIO m, Show e, Monad m, Error e) =>
    Monad (LogErrorT e m) where
    act >>= f = LogErrorT $ do result <- runLogErrorT act
                               case result of
                                 Left err -> return $ Left err
                                 Right val -> runLogErrorT $ f val
    fail = throwError . strMsg
    return = LogErrorT . return . Right

instance (MonadIO m, Show e, Error e, Monad m) =>
    MonadError e (LogErrorT e m) where
    throwError e = LogErrorT $ do liftIO $ print e
                                  return $ Left e
    catchError act f = LogErrorT $ do result <- runLogErrorT act
                                      case result of
                                        Left err -> runLogErrorT $ f err
                                        Right val -> return (Right val)

instance (MonadIO m, Show e, Error e, Monad m) =>
    MonadPlus (LogErrorT e m) where
    mplus a b = a `catchError` \_ -> b
    mzero = throwError $ strMsg "(mzero)"

instance (MonadIO m, Show e, Error e, Monad m) => MonadIO (LogErrorT e m) where
    liftIO ioact = LogErrorT $ do ioResult <- liftIO ioact
                                  return $ Right $ ioResult

