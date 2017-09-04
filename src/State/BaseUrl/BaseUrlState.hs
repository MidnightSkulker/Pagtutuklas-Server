{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XUndecidableInstances #-}
-- | Put together all the different flavours of state
module State.BaseUrl.BaseUrlState
       ( BaseUrlState (..)
       , HasBaseUrlState (..)
       , getRPXHost
       , getHostPort
       , MonadBaseUrl (..)
       ) where

import Control.Monad.Reader ( asks, ReaderT )
import Network.URI ( URI )
import Data.Cache.LRU.IO ( AtomicLRU )
import qualified Data.ByteString.Char8 as B
import Snap.Extension
import qualified Snap.Types as S
import State.CacheState ( CacheState (..), HasCacheState (..)
                        , CacheKey, fromCacheConfig, getCacheKey )
import State.DBState ( DBState, HasDBState (..), fromDBConfig )
import State.MinState ( MinState, HasMinState (..), fromMinConfig, getPort )
import qualified State.RPXState as RPX ( RPXState, HasRPXState (..), fromRPXConfig, getRPXHost )
import State.HandlerState ( HandlerState (..) )
import Config.BaseUrl.BaseUrlConfig ( BaseUrlConfig (..) )
import Config.HostPort ( HostPort )

data BaseUrlState = BaseUrlState { dbState        :: DBState
                                 , cacheState     :: CacheState
                                 , rpxState       :: RPX.RPXState
                                 , minState       :: MinState  
                                 }

instance Show BaseUrlState where
  show BaseUrlState { dbState = db, cacheState = c
                    , rpxState = rpx, minState = m } =
    "    DBState: " ++ show db ++ "\n" ++
    "    CacheState: " ++ show c ++ "\n" ++
    "    RPXState: " ++ show rpx ++ "\n" ++
    "    MinState: " ++ show m ++ "\n"

class HasBaseUrlState s where
    getBaseUrlState :: s -> BaseUrlState
    setBaseUrlState :: BaseUrlState -> s -> s
    modifyBaseUrlState :: (BaseUrlState -> BaseUrlState) -> s -> s
    modifyBaseUrlState f s = setBaseUrlState (f $ getBaseUrlState s) s

------------------------------------------------------------------------------
-- | The 'MonadBaseUrl' type class.
class S.MonadSnap m => MonadBaseUrl m where
    fishCacheKey     :: m ( AtomicLRU CacheKey URI )
    
------------------------------------------------------------------------------
instance InitializerState BaseUrlState where
    extensionId = const (B.pack "BaseUrl/BaseUrl")
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

------------------------------------------------------------------------------
instance HasBaseUrlState s => MonadBaseUrl (SnapExtend s) where
   fishCacheKey = do base <- asks getBaseUrlState
                     return ( getCacheKey ( getCacheState base ) )

------------------------------------------------------------------------------
instance (S.MonadSnap m, HasBaseUrlState s) => MonadBaseUrl (ReaderT s m) where
   fishCacheKey = do base <- asks getBaseUrlState
                     return ( getCacheKey ( getCacheState base ) )

-- | Fish more deeply for some RPX fields
getRPXHost :: (HasBaseUrlState st) => st -> String
getRPXHost st = RPX.getRPXHost (getBaseUrlState st)

-- | Fish more deeply for the port field
getHostPort :: (HasBaseUrlState st) => st -> HostPort
getHostPort st = getPort (getBaseUrlState st)

instance HasBaseUrlState BaseUrlState where
  getBaseUrlState = id
  setBaseUrlState s _b = s
  
instance RPX.HasRPXState BaseUrlState where
  getRPXState = rpxState

instance HasMinState BaseUrlState where
  getMinState = minState

instance HasCacheState BaseUrlState where
  getCacheState = cacheState
  setCacheState lru c = c { cacheState = lru }

instance HasDBState BaseUrlState where
  getDBState = dbState

instance HandlerState BaseUrlState BaseUrlConfig where
  fromCfg c = do dbSt      <- fromDBConfig (dbConfig c)
                 rpxSt     <- RPX.fromRPXConfig (rpxConfig c)
                 minSt     <- fromMinConfig (minConfig c)
                 cacheSt   <- fromCacheConfig (cacheConfig c)
                 return ( BaseUrlState { cacheState = cacheSt
                                       , dbState    = dbSt
                                       , rpxState   = rpxSt
                                       , minState   = minSt
                                       } )
