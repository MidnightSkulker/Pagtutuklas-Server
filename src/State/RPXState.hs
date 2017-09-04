-- | State information relevant to a RPX
module State.RPXState
    ( RPXState    ( .. )
    , MonadRPX ( .. )
    , HasRPXState ( .. )
    , fromRPXConfig
    , getRPXHost
    , getDefaultProto
    ) where

import qualified Snap.Types as S
import Snap.Extension
import Control.Monad.Reader ( asks, ReaderT )
import qualified Config.RPXConfig as C ( RPXConfig (..) )

-- | This data type contains all the proxy relevant state information
data RPXState = RPXState { defaultProto :: String
                         , rpxHost      :: String
                         } deriving ( Show )
  
-- | A class that characterizes having a proxy state
class HasRPXState s where
  getRPXState :: s -> RPXState

instance HasRPXState RPXState where
  getRPXState = id

-- | Create the RPX relevant state information from the RPX relevant
--   configuration information.
fromRPXConfig :: C.RPXConfig -> IO RPXState
fromRPXConfig cfg = do
  putStrLn ( ">>>> processRPXConfig: " ++ show cfg )
  return ( RPXState { defaultProto  = C.defaultProto cfg
                    , rpxHost       = C.rpxHost cfg } )

-- | Fish the RPX host of the application state
getRPXHost :: (HasRPXState st) => st -> String
getRPXHost st = rpxHost (getRPXState st)

-- | Fish the default protocol out of the application state
getDefaultProto :: (HasRPXState st) => st -> String
getDefaultProto st = defaultProto (getRPXState st)

------------------------------------------------------------------------------
-- | The 'MonadRPX' type class.
class S.MonadSnap m => MonadRPX m where
    -- | Get the RPX host out of the monad plumbing
    fishRPXHost      :: m String
    -- | Get the Default Protocol out of the monad plumbing
    fishDefaultProto :: m String
    
instance HasRPXState s => MonadRPX (SnapExtend s) where
   fishRPXHost = do rpx <- asks getRPXState
                    return ( getRPXHost rpx )
   fishDefaultProto = do rpx <- asks getRPXState
                         return ( getDefaultProto rpx )

instance (S.MonadSnap m, HasRPXState s) => MonadRPX (ReaderT s m) where
   fishRPXHost = do rpx <- asks getRPXState
                    return ( getRPXHost rpx )
   fishDefaultProto = do rpx <- asks getRPXState
                         return ( getDefaultProto rpx )
