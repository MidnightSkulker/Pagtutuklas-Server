-- | State information relevant to a proxy server
module State.ProxyState
    ( ProxyState    ( .. )
    , HasProxyState ( .. )
    , MonadProxy ( .. )
    , fromProxyConfig
    , getProxyHost
    ) where

import qualified Snap.Types as S
import Snap.Extension
import Control.Monad.Reader ( asks, ReaderT )
import qualified Config.ProxyConfig as C ( ProxyConfig (..) )
import Config.HostPort ( HostPort )

-- | This data type contains all the proxy relevant state information
data ProxyState = ProxyState { proxyHostPort :: Maybe HostPort } deriving ( Show )
  
-- | A class that characterizes having a proxy state
class HasProxyState s where
  getProxyState :: s -> ProxyState

instance HasProxyState ProxyState where
  getProxyState = id

-- | Create the Proxy relevant state information from the Proxy relevant
--   configuration information.
fromProxyConfig :: C.ProxyConfig -> IO ProxyState
fromProxyConfig cfg = return ( ProxyState { proxyHostPort = C.proxyHostPort cfg } )

-- | Fish the proxy host of the application state
getProxyHost :: (HasProxyState st) => st -> Maybe HostPort
getProxyHost st = proxyHostPort (getProxyState st)

------------------------------------------------------------------------------
-- | The 'MonadProxy' type class.
class S.MonadSnap m => MonadProxy m where
  -- | Get the Proxy host out of the monad plumbing
  fishProxyHostPort :: m (Maybe HostPort)
    
instance HasProxyState s => MonadProxy (SnapExtend s) where
  fishProxyHostPort = do p <- asks getProxyState
                         return ( getProxyHost p )

instance (S.MonadSnap m, HasProxyState s) => MonadProxy (ReaderT s m) where
  fishProxyHostPort = do p <- asks getProxyState
                         return ( getProxyHost p )
