{-# OPTIONS -XFlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.
-}

module BaseUrl.Site ( site ) where

-- Haskell and hackage imports
import           Control.Applicative ( (<$>), (<|>) )
import           Data.Maybe ( fromMaybe )
import qualified Data.Text.Encoding as T
-- Snap imports
import           Snap.Extension.Heist ( heistLocal, render )
import           Snap.Extension.Timer ( startTimeSplice, currentTimeSplice )
import           Snap.Extension.Heist.Impl ( HasHeistState (..) )
import           Snap.Util.FileServe ( serveDirectory )
import qualified Snap.Types as S ( ifTop, getParam, route )
import           Text.Templating.Heist ( bindSplices, bindString )
-- Local imports
import           BaseUrl.Application ( Application )
import           Handler.BaseUrl.BaseUrl ( baseUrl )
import           State.BaseUrl.BaseUrlState ( HasBaseUrlState )
import           State.RPXState ( HasRPXState )
import           Snap.Extension.Timer.Impl ( HasTimerState (..) )

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: ( HasTimerState st, HasHeistState (Application st) st ) =>
         Application st ()
index = S.ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: ( HasTimerState st, HasHeistState (Application st) st ) =>
        Application st ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> S.getParam p


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: ( HasTimerState st, HasHeistState (Application st) st
        , HasBaseUrlState st, HasRPXState st ) =>
        Application st ()
site = S.route [ ( "/"                , index   )
               , ( "/echo/:stuff"     , echo    )
               , ( "jsapi/v3/base_url", baseUrl )
               ]
       <|> serveDirectory "resources/static"
