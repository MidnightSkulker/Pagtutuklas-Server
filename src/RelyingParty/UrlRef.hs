module RelyingParty.UrlRef
    ( loadOne
    , UrlRef(..)
    ) where

import Database.Database ( quickFetch, bsToString )
import Database.HDBC ( SqlValue(..) )
import Application ( Application )
import State.DBState ( HasDBState )

data UrlRef = UrlRef { url :: String
                     , identifierId :: Integer
                     , relyingPartyId :: Integer
                     , xdReceiverUrl :: Maybe String
                     , provider :: Maybe String
                     }

loadOne :: ( HasDBState st ) => Integer -> Application st (Maybe UrlRef)
loadOne urlRefId = do
  let query = "SELECT u.url AS url, identifier_id, relying_party_id, \
              \x.url AS xd_receiver_url, swp.social_widget_name \
              \FROM urls u \
              \JOIN url_refs uref ON u.url_id = uref.url_id \
              \LEFT JOIN urls x ON uref.xd_receiver_url_id = x.url_id \
              \LEFT JOIN social_widget_providers swp ON uref.social_widget_provider_id = swp.id \
              \WHERE url_ref_id = ?"

  results <- quickFetch query [SqlInteger urlRefId]

  case results of
    [] -> return Nothing
    -- Iphone library creates these bastard URL refs with NULL
    -- identifiers.  Analytics can't really process them at the
    -- moment, so just drop them on the floor.
    [[_, SqlNull, _, _, _]] -> return Nothing
    --
    [[SqlByteString urlStr, SqlInteger identId, SqlInteger rpId, mXdrcvr, mProv]] ->
        return ( Just UrlRef { url = bsToString urlStr
                             , identifierId = identId
                             , relyingPartyId = rpId
                             , xdReceiverUrl = xdrcvr
                             , provider = prov
                             } )
        where
          xdrcvr = case mXdrcvr of
                    SqlByteString str -> Just $ bsToString str
                    SqlNull -> Nothing
                    _ -> fail $ "Expected a (string, integer, integer, \
                                \maybe string, maybe string) \
                                \when loading URL ref. Got: " ++ show results
          prov =  case mProv of
                    SqlByteString str -> Just $ bsToString str
                    SqlNull -> Nothing
                    _ -> fail $ "Expected a (string, integer, integer, \
                                \maybe string, maybe string) \
                                \when loading URL ref. Got: " ++ show results
    --
    _ -> fail $ "Expected a (string, integer, integer, \
                \maybe string, maybe string) \
                \when loading URL ref. Got: " ++ show results
