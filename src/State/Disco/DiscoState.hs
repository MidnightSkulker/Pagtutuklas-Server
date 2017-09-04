{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XUndecidableInstances #-}
-- | Put together all the different flavours of state
module State.Disco.DiscoState
       ( DiscoState (..)
       , HasDiscoState (..)
       , getRPXHost
       , getHostPort
       , MonadDisco
       ) where

-- Haskell and hackage imports
import Control.Monad.Reader ( ReaderT )
import qualified Data.ByteString.Char8 as B
import qualified Snap.Types as S
import Snap.Extension
-- Local imports
import State.MinState ( MinState, HasMinState (..), fromMinConfig, getPort )
import State.ProxyState ( ProxyState, HasProxyState (..), fromProxyConfig )
import qualified State.RPXState as RPX ( RPXState, HasRPXState (..), fromRPXConfig, getRPXHost )
import State.HandlerState ( HandlerState (..) )
import Config.Disco.DiscoConfig ( DiscoConfig (..) )
import Config.HostPort ( HostPort )

data DiscoState = DiscoState { rpxState       :: RPX.RPXState
                             , minState       :: MinState  
                             , proxyState     :: ProxyState
                             }

instance Show DiscoState where
  show DiscoState { rpxState = rpx, minState = m, proxyState = p } =
    "\n" ++
    "    RPXState: " ++ show rpx ++ "\n" ++
    "    MinState: " ++ show m ++ "\n"   ++
    "    ProxyState: " ++ show p ++ "\n"

class HasDiscoState s where
    getDiscoState :: s -> DiscoState
    setDiscoState :: DiscoState -> s -> s
    modifyDiscoState :: (DiscoState -> DiscoState) -> s -> s
    modifyDiscoState f s = setDiscoState (f $ getDiscoState s) s

------------------------------------------------------------------------------
-- | The 'MonadDisco' type class.
class S.MonadSnap m => MonadDisco m where
    
------------------------------------------------------------------------------
instance InitializerState DiscoState where
    extensionId = const (B.pack "Disco/Disco")
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

------------------------------------------------------------------------------
instance HasDiscoState s => MonadDisco (SnapExtend s) where

------------------------------------------------------------------------------
instance (S.MonadSnap m, HasDiscoState s) => MonadDisco (ReaderT s m) where

-- | Fish more deeply for some RPX fields
getRPXHost :: (HasDiscoState st) => st -> String
getRPXHost st = RPX.getRPXHost (getDiscoState st)

-- | Fish more deeply for the port field
getHostPort :: (HasDiscoState st) => st -> HostPort
getHostPort st = getPort (getDiscoState st)

instance HasDiscoState DiscoState where
  getDiscoState = id
  setDiscoState s _b = s

instance RPX.HasRPXState DiscoState where
  getRPXState = rpxState

instance HasProxyState DiscoState where
  getProxyState = proxyState

instance HasMinState DiscoState where
  getMinState = minState

instance HandlerState DiscoState DiscoConfig where
  fromCfg c = do rpxSt     <- RPX.fromRPXConfig (rpxConfig c)
                 minSt     <- fromMinConfig (minConfig c)
                 proxySt   <- fromProxyConfig (proxyConfig c)
                 return ( DiscoState { rpxState       = rpxSt
                                     , minState       = minSt
                                     , proxyState     = proxySt } )
