-- | State information relevant to URL resolution function
module State.ResolverState
    ( ResolverState ( .. )
    , HasResolverState ( .. )
    , MonadResolver ( .. )
    , makeResolverState
    , fromResolverConfig
    , getResolver
    ) where

import ADNS                  ( Resolver, initResolver, InitFlag (..) )
import Control.Monad.Reader  ( asks, ReaderT )
import qualified Snap.Types as S
import Snap.Extension
import Config.ResolverConfig ( ResolverConfig (..) )

-- | This data type contains all the strictly resolver relevant
--   state information
data ResolverState = ResolverState { stResolver :: Resolver }
  
class HasResolverState s where
  getResolverState :: s -> ResolverState

instance HasResolverState ResolverState where
  getResolverState = id

-- | Build a new resolver state
makeResolverState :: Resolver -> ResolverState
makeResolverState r = ResolverState { stResolver = r }

-- | Fish the resolver out of the resolver state
getResolver :: (HasResolverState st) => st -> Resolver
getResolver st = stResolver ( getResolverState st )

-- | Build resolver state from configuration information
fromResolverConfig :: ResolverConfig -> IO ResolverState
fromResolverConfig _cfg = do
  resolver <- initResolver [NoErrPrint, NoServerWarn] (\r -> return r)
  return ResolverState { stResolver = resolver }

------------------------------------------------------------------------------
-- | The 'MonadResolver' type class.
class S.MonadSnap m => MonadResolver m where
  -- | Get the resolver out of the monad plumbing
  fishResolver :: m Resolver

instance HasResolverState s => MonadResolver (SnapExtend s) where
   fishResolver = do r <- asks getResolverState
                     return ( getResolver r )

instance (S.MonadSnap m, HasResolverState s) => MonadResolver (ReaderT s m) where
   fishResolver = do r <- asks getResolverState
                     return ( getResolver r )
