{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XUndecidableInstances #-}
-- | Put together all the different flavours of state
module State.RpxMe.RpxMeState
       ( RpxMeState (..)
       , HasRpxMeState (..)
       , getRPXHost
       , getHostPort
       , MonadRpxMe
       ) where

-- Haskell and hackage imports
import Control.Monad.Reader ( ReaderT )
import qualified Data.ByteString.Char8 as B
import qualified Snap.Types as S
import Snap.Extension
-- Local imports
import State.DBState ( DBState, HasDBState (..), fromDBConfig )
import State.MinState ( MinState, HasMinState (..), fromMinConfig, getPort )
import qualified State.RPXState as RPX ( RPXState, HasRPXState (..), fromRPXConfig, getRPXHost )
import State.BeanstalkState ( BeanstalkState, HasBeanstalkState ( .. )
                            , fromBeanstalkConfig ) 
import State.HandlerState ( HandlerState (..) )
import Config.RpxMe.RpxMeConfig ( RpxMeConfig (..) )
import Config.HostPort ( HostPort )

data RpxMeState = RpxMeState { dbState        :: DBState
                             , beanstalkState :: BeanstalkState
                             , rpxState       :: RPX.RPXState
                             , minState       :: MinState  
                             }

instance Show RpxMeState where
  show RpxMeState { dbState = db, beanstalkState = b
                    , rpxState = rpx, minState = m } =
    "\n" ++
    "    DBState: " ++ show db ++ "\n" ++
    "    BeanstalkState:\n" ++ show b ++ "\n" ++
    "    RPXState: " ++ show rpx ++ "\n" ++
    "    MinState: " ++ show m ++ "\n"

class HasRpxMeState s where
    getRpxMeState :: s -> RpxMeState
    setRpxMeState :: RpxMeState -> s -> s
    modifyRpxMeState :: (RpxMeState -> RpxMeState) -> s -> s
    modifyRpxMeState f s = setRpxMeState (f $ getRpxMeState s) s

------------------------------------------------------------------------------
-- | The 'MonadRpxMe' type class.
class S.MonadSnap m => MonadRpxMe m where
    
------------------------------------------------------------------------------
instance InitializerState RpxMeState where
    extensionId = const (B.pack "RpxMe/RpxMe")
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

------------------------------------------------------------------------------
instance HasRpxMeState s => MonadRpxMe (SnapExtend s) where

------------------------------------------------------------------------------
instance (S.MonadSnap m, HasRpxMeState s) => MonadRpxMe (ReaderT s m) where

-- | Fish more deeply for some RPX fields
getRPXHost :: (HasRpxMeState st) => st -> String
getRPXHost st = RPX.getRPXHost (getRpxMeState st)

-- | Fish more deeply for the port field
getHostPort :: (HasRpxMeState st) => st -> HostPort
getHostPort st = getPort (getRpxMeState st)

instance HasRpxMeState RpxMeState where
  getRpxMeState = id
  setRpxMeState s _b = s

instance RPX.HasRPXState RpxMeState where
  getRPXState = rpxState

instance HasMinState RpxMeState where
  getMinState = minState

instance HasBeanstalkState RpxMeState where
  getBeanstalkState = beanstalkState
  setBeanstalkState r b = b { beanstalkState = r }

instance HasDBState RpxMeState where
  getDBState = dbState

instance HandlerState RpxMeState RpxMeConfig where
  fromCfg c = do dbSt      <- fromDBConfig (dbConfig c)
                 rpxSt     <- RPX.fromRPXConfig (rpxConfig c)
                 minSt     <- fromMinConfig (minConfig c)
                 beanstalkSt <- fromBeanstalkConfig (beanstalkConfig c)
                 return ( RpxMeState { beanstalkState = beanstalkSt
                                     , dbState        = dbSt
                                     , rpxState       = rpxSt
                                     , minState       = minSt
                                     } )
