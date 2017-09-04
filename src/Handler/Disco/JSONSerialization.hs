{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Disco.JSONSerialization () where

import Network.OpenID.JanRain.Endpoint
import Network.OpenID.JanRain.Identifier ( Identifier, parse )
import Text.JSON
import Network.URI
import Network.URL

instance JSON URI where
    showJSON = showJSON . show
    readJSON (JSString jstr) =
        maybe (Error "Not a URI") Ok $ parseURI $ fromJSString jstr
    readJSON _ = Error "Expected a string for a URI"

instance JSON Identifier where
    showJSON = showJSON . show
    readJSON (JSString jstr) =
        maybe (Error "Not a URI or XRI") Ok $ parse $ fromJSString jstr
    readJSON _ = Error "Expected a string for a URI or XRI"

instance JSON Endpoint where
    showJSON e = makeObj [ "local_id" ~~? localId
                         , "claimed_id" ~~? claimedId
                         , "server_url" ~~ serverEndpoint
                         , "type_uris" ~~ typeUris
                         , "display_identifier" ~~? displayIdentifier
                         , "used_yadis" ~~ usedYadis
                         ]
        where name ~~ f = (name, showJSON (f e))
              name ~~? f = name ~~ (maybeToJS . f)
              maybeToJS :: JSON v => Maybe v -> JSValue
              maybeToJS = maybe JSNull showJSON
    readJSON _ = Error "Reading endpoints not implemented"

instance JSON URL where
    showJSON = showJSON . exportURL
    readJSON (JSString jstr) =
        maybe (Error "Not a URL") Ok $ importURL $ fromJSString jstr
    readJSON _ = Error "Expected a string for a URL"
