{-# OPTIONS -XScopedTypeVariables -XFunctionalDependencies  #-}
-- | State information relevant to BaseUrl caching
module State.CacheState
    ( CacheState ( .. )
    , MonadCache ( .. )
    , HasCacheState ( .. )
    , fromCacheConfig
    , CacheKey
    , getCacheKey
    ) where

-- Haskell and Hackage
import Data.Cache.LRU.IO ( AtomicLRU, newAtomicLRU )
import Network.URI ( URI )
import Network.URL ( URL )
import qualified Data.ByteString.Char8 as B
import qualified Snap.Types as S
import Snap.Extension
import Control.Monad.Reader ( asks, ReaderT )
-- Janrain
import Config.CacheConfig ( CacheConfig )

type CacheKey = ( B.ByteString, Maybe URL )

-- | This data type contains all the strictly baseUrl relevant
--   state information
data CacheState = CacheState { stCache :: AtomicLRU CacheKey URI }

instance Show CacheState where
  show _c = "CacheState (Not much to show)"

class HasCacheState s where
    getCacheState :: s -> CacheState
    setCacheState :: CacheState -> s -> s
    modifyCacheState :: (CacheState -> CacheState) -> s -> s
    modifyCacheState f s = setCacheState (f $ getCacheState s) s

instance HasCacheState CacheState where
    getCacheState = id
    setCacheState _c s = s
    
-- | Convert the baseUrl relevant config information into the BaseURL
--   relevant state
fromCacheConfig :: CacheConfig -> IO CacheState
fromCacheConfig _cfg = 
  do cacheSt <- newAtomicLRU $ Just ( 1024 * 8 )
     return ( CacheState { stCache = cacheSt } )

-- | Fish the base URL cache key out of the application state
getCacheKey :: (HasCacheState st) => st -> AtomicLRU CacheKey URI
getCacheKey st = stCache ( getCacheState st )

------------------------------------------------------------------------------
-- | The 'MonadCache' type class.
class S.MonadSnap m => MonadCache m where
  -- | Get the listening port out of the monad plumbing
  fishCacheKey :: m ( AtomicLRU CacheKey URI )

instance HasCacheState s => MonadCache (SnapExtend s) where
  fishCacheKey = do rpx <- asks getCacheState
                    return ( getCacheKey rpx )

instance (S.MonadSnap m, HasCacheState s) => MonadCache (ReaderT s m) where
  fishCacheKey = do rpx <- asks getCacheState
                    return ( getCacheKey rpx )
