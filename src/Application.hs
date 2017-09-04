{-
This module defines our application's monad and any application-specific
information it requires.
-}

module Application ( Application ) where

-- Snap imports
import Snap.Extension ( SnapExtend )

------------------------------------------------------------------------------
-- | 'Application' is our application's monad. This is a general purpose
--    application useable by any handlers.
type Application st = SnapExtend st
