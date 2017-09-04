{-# OPTIONS -XFlexibleInstances #-}
{-
This module defines our application's monad and any application-specific
information it requires.
-}

module Disco.Application
  ( Application
  , DiscoApplication
  , applicationInitializer
  ) where

-- Snap imports
import Snap.Extension ( Initializer )
import Snap.Extension.Heist.Impl ( HeistState, HasHeistState (..), heistInitializer )
import Snap.Extension.Timer.Impl ( TimerState, HasTimerState (..), timerInitializer )
-- Local imports
import State.Disco.DiscoState ( DiscoState, HasDiscoState ( .. ) )
-- import qualified State.RPXState as R ( HasRPXState (..) )
import qualified State.ProxyState as P ( HasProxyState (..) )
import qualified State.MinState as M ( HasMinState (..) )
import Application ( Application )

------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
-- And for Disco, it adds Disco specific state
type DiscoApplication = Application ApplicationState

------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by
--    the Snap extensions we're using.  We're using Heist so we can easily
--    render Heist templates, and Timer simply to illustrate the config
--    loading differences between development and production modes.
data ApplicationState = ApplicationState
    { templateState :: HeistState (Application ApplicationState)
    , timerState    :: TimerState
    , discoState    :: DiscoState
    }

------------------------------------------------------------------------------
instance HasDiscoState ApplicationState where
    getDiscoState     = discoState
    setDiscoState s a = a { discoState = s }

-- instance R.HasRPXState ApplicationState where
--     getRPXState = R.getRPXState . getDiscoState
    
instance M.HasMinState ApplicationState where
    getMinState = M.getMinState . getDiscoState

instance P.HasProxyState ApplicationState where
    getProxyState = P.getProxyState . getDiscoState

------------------------------------------------------------------------------
instance HasHeistState (Application ApplicationState) ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

------------------------------------------------------------------------------
instance HasTimerState ApplicationState where
    getTimerState     = timerState
    setTimerState s a = a { timerState = s }

-- | The 'Initializer' for ApplicationState. For more on 'Initializer's,
--   see the documentation from the snap package. Briefly, this is used
--   to generate the 'ApplicationState' needed for our application and
--   will automatically generate reload\/cleanup actions for us which we
--   don't need to worry about.
--
--   As a result of initializing the DiscoState:
--   1. There is a connection established to the database.
--   2. The RPX state is initialized from some of the configuration
--      information.
--   3. A new LRU cache is established
applicationInitializer :: DiscoState -> Initializer ApplicationState
applicationInitializer st = do
    heist <- heistInitializer "resources/templates"
    timer <- timerInitializer
    return $ ApplicationState { templateState = heist 
                              , timerState    = timer
                              , discoState    = st
                              }
