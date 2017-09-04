{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -XFunctionalDependencies -XMultiParamTypeClasses #-}
-- | Minimal state information for a handler.
module State.MinState
    ( MinState ( .. )
    , MonadMin ( .. )
    , HasMinState ( .. )
    , fromMinConfig
    , getPort
    ) where

import qualified Snap.Types as S
import Snap.Extension
import Control.Monad.Reader ( asks, ReaderT )
import Config.HostPort ( HostPort )
import Config.MinConfig ( MinConfig (..) )

-- | Minimum state information for a handler
data MinState = MinState { port :: HostPort } deriving ( Show )

instance HasMinState MinState where
  getMinState = id
  
-- | HasMinState characterizes having at least the minimal state
--   information for a handler in the discovery server.
class HasMinState st where
  getMinState :: st -> MinState

-- | Create minimum state information
fromMinConfig :: MinConfig -> IO MinState
fromMinConfig cfg = return MinState { port = listen cfg }

-- | Get the port out of the minstate
getPort :: (HasMinState st) => st -> HostPort
getPort st = port ( getMinState st )

------------------------------------------------------------------------------
-- | The 'MonadMin' type class.
class S.MonadSnap m => MonadMin m where
    -- | Get the listening port out of the monad plumbing
    fishListen      :: m HostPort

instance HasMinState s => MonadMin (SnapExtend s) where
   fishListen = do rpx <- asks getMinState
                   return ( getPort rpx )

instance (S.MonadSnap m, HasMinState s) => MonadMin (ReaderT s m) where
   fishListen = do rpx <- asks getMinState
                   return ( getPort rpx )
