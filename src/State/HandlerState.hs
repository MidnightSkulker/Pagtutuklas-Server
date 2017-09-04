{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies #-}
-- | A handler state that is completely polymorphic, no dependence
--   on a given state component.
module State.HandlerState ( HandlerState (..) ) where

-- | Characterize a state as something computed from config
class HandlerState st c | st -> c where
  fromCfg :: c -> IO st
