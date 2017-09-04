{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS -XScopedTypeVariables #-}
module Discover.Discover
    ( discoverWith
    , discoverAsClaimedId
    , discoverInitial
    , discoverAsOp
    ) where

import Data.URLEncoded ( (%?) )
import qualified Text.ParserCombinators.Parsec as Parsec ( parse )
import Control.Arrow ( (>>>) )
import qualified Network.OpenID.JanRain.Identifier as Id ( Identifier (..), parse )
import qualified Network.OpenID.JanRain.Endpoint as E ( Endpoint (..), EndUserInfo (..) )
import Fetcher.Monad ( MonadFetch (..) ) -- httpGet is from this
import qualified Fetcher.Response as Response ( Response (..) )
import Network.OpenID.JanRain.Common ( xrdsContentType )
import Discover.HTML ( openIdLinkTags, EndpointInfo(..), OpenIDVersion(..), xrdsLocationFromHtml )
import Fetcher.Errors ( badOpenIDMetadata, notAnOpenID, DiscoveryError
                      , catchFailure, firstSucceeding, invalidInput )
import qualified Network.HTTP as HTTP ( Header (..), lookupHeader, HeaderName (..) )
import Network.URI ( URI, parseURI, relativeTo, uriRegName, uriAuthority 
                   , parseRelativeReference )
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Control.Monad ( mplus, when, MonadPlus )
import Control.Monad.Error ( MonadError )
import Data.Char ( toLower )
import Discover.WhiteSpace ( strip )
import Network.OpenID.JanRain.Version ( openId1_1Type, openId2Type )
import Discover.HostMeta ( siteXrdsUris, uriTemplateFromLinkPatterns )
import qualified Discover.HostMeta as DiscoverHostMeta ( hostMetaUri )
import Discover.XRDS
    ( handleXrdsContentWith
    , extractOpEndpoints
    , extractUserEndpoints
    , extractUserEndpointsXRI
    , extractSiteXrdsOpEndpoints
    , extractUriTemplate
    )
import Discover.URITemplate ( fillURITemplate )
import qualified Text.HostMeta as THM ( FieldParser, hostMetaParser
                                      , linkParser, linkPatternFieldParser )
import qualified Control.Monad.Trace as Trace ( frame, log )
import Control.Monad.Trace ( frame, MonadTracedErrors, EmbedsMPlus )

--------------------------------------------------------------------------------

discoverWith :: (DiscoveryError e, MonadError e m) =>
                (Id.Identifier -> m [E.Endpoint]) -> String -> m [E.Endpoint]
discoverWith disco userInput =
    case Id.parse userInput of
      Nothing -> invalidInput userInput
      Just ident -> disco ident

type HostMetaUri = URI

googleFallbackUrl :: String
googleFallbackUrl =
    "https://www.google.com/accounts/o8/host-meta?hd="

googleFallbackUri :: (MonadError e m) => URI -> m URI
googleFallbackUri uri = do
  domain <- maybe (fail $ "URI has no domain: " ++ show uri)
                  (return . uriRegName)
                  (uriAuthority uri)

  maybe (fail $ "Failed to parse fallback URL with domain: " ++ show uri)
        return
        (parseURI $ googleFallbackUrl ++ domain)

--------------------------------------------------
-- Identifier discovery methods
discoverAsClaimedId, discoverInitial, discoverAsOp
    :: (MonadTracedErrors msg e m,
        MonadFetch msg e m,
        DiscoveryError e,
        EmbedsMPlus msg) =>
       Id.Identifier -> m [E.Endpoint]

discoverInitial (Id.IdURI uri) = discoverInitialURI uri
discoverInitial (Id.IdXRI s) = discoverXRI s

discoverAsClaimedId (Id.IdURI uri) = discoverAsClaimedIdURI uri
discoverAsClaimedId (Id.IdXRI s) = discoverXRI s

discoverAsOp (Id.IdURI uri) = discoverAsOpURI uri
discoverAsOp (Id.IdXRI _) = return []

--------------------------------------------------
-- URI discovery methods

discoverInitialURI, discoverAsClaimedIdURI, discoverAsOpURI
    :: (MonadTracedErrors msg e m,
        MonadFetch msg e m,
        DiscoveryError e,
        EmbedsMPlus msg) =>
       URI -> m [E.Endpoint]

discoverInitialURI uri =
    let hostMetaUri = DiscoverHostMeta.hostMetaUri uri
    in do eps <- Trace.frame ("discoverInitial " ++ show uri)
                 $ firstSucceeding
                 $ [ discoverHostMeta uri hostMetaUri
                   , discoverUriAsOp uri
                   , discoverUriAsClaimedId uri
                   , discoverHostMeta uri =<< googleFallbackUri uri
                   ]
          Trace.log "Successfully found endpoints"
          return eps

-- |Given a URI, do host-meta discovery on the URI, treating it as the
-- claimed ID.  If host-meta discovery fails, perform normal OpenID
-- discovery on the URI.
discoverAsClaimedIdURI uri =
    let hostMetaUri = DiscoverHostMeta.hostMetaUri uri
    in do eps <- frame ("Discover as claimed id: " ++ show uri)
                 $ firstSucceeding
                 $ [ discoverHostMetaAsClaimedId uri hostMetaUri
                   , discoverUriAsClaimedId uri
                   , discoverHostMetaAsClaimedId uri =<< googleFallbackUri uri
                   ]
          Trace.log "Discovery successful"
          return eps


discoverAsOpURI uri =
    let hostMetaUri = DiscoverHostMeta.hostMetaUri uri
    in do eps <- frame ("Discover as OP: " ++ show uri) $
                 firstSucceeding $
                [ discoverHostMeta uri hostMetaUri
                , discoverUriAsOp uri
                , discoverHostMeta uri =<< googleFallbackUri uri
                ]
          Trace.log "Discovery successful"
          return eps

--------------------------------------------------
-- XRI discovery

xriProxyUri :: URI
Just xriProxyUri = parseURI "http://proxy.xri.net/"

discoverXRI :: (MonadTracedErrors msg e m, MonadFetch msg e m,
                DiscoveryError e, EmbedsMPlus msg) =>
               String -> m [E.Endpoint]
discoverXRI iname =
    frame "XRI discovery" $ do
      uri <- case parseRelativeReference $ "/" ++ iname of
               Nothing -> invalidInput iname
               Just rel -> let Just base = rel `relativeTo` xriProxyUri
                           in return $ base %? ("_xrd_r", xrdsContentType)

      resp <- httpGet (uri, [acceptHeader xrdsContentType])
      handleXrdsContentWith (extractUserEndpointsXRI iname) $ Response.body resp

--------------------------------------------------

acceptHeader :: String -> HTTP.Header
acceptHeader = HTTP.Header HTTP.HdrAccept

claimedIdAcceptHeader :: HTTP.Header
claimedIdAcceptHeader = acceptHeader
                        "application/xrds+xml, application/xhtml+xml; q=0.5, \
                        \text/html; q=0.3"

discoverUriAsOp :: (MonadTracedErrors msg e m, DiscoveryError e, MonadFetch msg e m, EmbedsMPlus msg) =>
                   URI -> m [E.Endpoint]
discoverUriAsOp uri = frame "OP YADIS discovery" $ do
  resp <- httpGet (uri, [claimedIdAcceptHeader])
  let body = Response.body resp
      handleXrds = handleXrdsContentWith extractOpEndpoints
  firstSucceeding [ frame "XRDS" $ getXrdsXmlStr resp >>= handleXrds
                  , frame "Fall back to XRDS" $ handleXrds body
                  ]

discoverUriAsClaimedId :: (MonadTracedErrors msg e m, DiscoveryError e, MonadFetch msg e m, EmbedsMPlus msg) =>
                          URI -> m [E.Endpoint]
discoverUriAsClaimedId uri =
    frame "Claimed ID discovery" $ do
      resp <- httpGet (uri, [claimedIdAcceptHeader])
      let body = Response.body resp
          claimedId = Response.uri resp
          handleXrds = handleXrdsContentWith (extractUserEndpoints claimedId)

      firstSucceeding [ frame "YADIS" $ getXrdsXmlStr resp >>= handleXrds
                      , frame "HTML" $ handleAsHtml uri
                      , frame "Fallback to XRDS" $ handleXrds body
                      ]

-- |Perform host-meta discovery on the given 'URI', extracting only OP
-- endpoints. Fetch and parse the resulting site XRDS, returning the
-- resulting 'E.Endpoint's.
discoverHostMeta :: (DiscoveryError e, MonadFetch msg e m) =>
                    URI -> HostMetaUri -> m [E.Endpoint]
discoverHostMeta uri hmUri =
    Trace.frame ("OP host-meta using URI " ++ show hmUri) $
    fetchSiteXrds hmUri >>=
                  handleXrdsContentWith (extractSiteXrdsOpEndpoints uri)

parseHostMeta :: (DiscoveryError e, MonadError e m) =>
                 THM.FieldParser a -> String -> m [a]
parseHostMeta parseField body = do
  let parser = THM.hostMetaParser parseField
      parseResult = Parsec.parse parser "<fetched host-meta doc>" body
      parseFailure e = badOpenIDMetadata $
                       "host-meta file failed to parse: " ++ show e
  either parseFailure return parseResult

-- |Use the host-meta file to find a Site XRDS file
fetchSiteXrds :: (DiscoveryError e, MonadFetch msg e m) =>
                 HostMetaUri -> m String
fetchSiteXrds hostMetaUri = do
  let nohdrs :: [HTTP.Header] = []  
  str :: String <- httpGetContents ( hostMetaUri, nohdrs )
  links <- parseHostMeta THM.linkParser str
  let uris = siteXrdsUris links
  when (null uris) $
       notAnOpenID $ "No Links found in host-meta:" ++ show hostMetaUri
  httpGetContents ( head uris, nohdrs )

{-| Given a URI template string and a 'URI',

  * fill in the template with the URI

  * fetch the resulting URI and parse it as an XRDS document

  * return the resulting list of 'E.Endpoint's
-}
discoverUserIdentifierFromTemplate ::
    (MonadTracedErrors msg e m, MonadFetch msg e m, DiscoveryError e) =>
    String -> URI -> m [E.Endpoint]
discoverUserIdentifierFromTemplate template identifierUri = do
  let nohdrs :: [HTTP.Header] = []    
  userXrdsLoc <- fillURITemplate template identifierUri
                 `catchFailure` \msg ->
                     badOpenIDMetadata
                     $ "host-meta had an invalid link pattern: " ++ msg
  handleXrdsContentWith (extractUserEndpoints identifierUri)
      =<< httpGetContents ( userXrdsLoc, nohdrs )

-- |Perform host-meta discovery on the given 'URI', treating it as the
-- claimed ID.  Look for a Link-Pattern header or URITemplate element
-- where appropriate and fetch and parse the resulting user XRDS,
-- returning the resulting 'E.Endpoint's.
discoverHostMetaAsClaimedId ::
    (MonadTracedErrors msg e m, DiscoveryError e, MonadFetch msg e m) =>
    URI -> HostMetaUri -> m [E.Endpoint]
discoverHostMetaAsClaimedId uri hostMetaUri =
    frame "Discover Claimed ID using host-meta" $ do
      let nohdrs :: [HTTP.Header] = []
      hostMetaContent <- httpGetContents (hostMetaUri, nohdrs )
      linkPatterns <-
          parseHostMeta THM.linkPatternFieldParser hostMetaContent
            `catchFailure` \msg ->
                badOpenIDMetadata $ "Failed to parse host-meta file: " ++ msg

      template' <- if (null linkPatterns)
                   then do
                     xrdsContent <- fetchSiteXrds hostMetaUri
                     extractUriTemplate uri xrdsContent
                   else return $ uriTemplateFromLinkPatterns linkPatterns

      case template' of
        Nothing -> notAnOpenID $
                   "Could not find URITemplate in host-meta discovery for: " ++
                   show uri
        Just template -> discoverUserIdentifierFromTemplate template uri

htmlAcceptHeader :: HTTP.Header
htmlAcceptHeader = HTTP.Header HTTP.HdrAccept "application/xml+xhtml, text/html; q=0.9"

handleAsHtml :: (MonadFetch msg e m, DiscoveryError e, MonadError e m) =>
                URI -> m [E.Endpoint]
handleAsHtml uri = do
    resp <- httpGet (uri, [htmlAcceptHeader])

    let theBody = Response.body resp
        theClaimedId = Response.uri resp
        result = discovered theClaimedId theBody
    when (null result) $ notAnOpenID "No endpoints found in HTML"
    return result
    where
      discovered claimedId body = map (toEndpoint claimedId) $ openIdLinkTags body
      toEndpoint claimedId (EndpointInfo ver serverUri mDelegate) =
          let userInfo =
                  E.EndUserInfo { E.euiClaimedId = Id.IdURI claimedId
                                , E.euiLocalId = fromMaybe (show claimedId) mDelegate
                                , E.euiDisplayIdentifier = show claimedId
                                }
              typeUris = case ver of
                           OpenID1 -> [openId1_1Type]
                           OpenID2 -> [openId2Type]
          in E.Endpoint { E.endUserInfo = Just userInfo
                        , E.serverEndpoint = serverUri
                        , E.typeUris = typeUris
                        , E.usedYadis = False
                        }

-- |The key for the XRDS-Location header
xrdsHeaderName :: String
xrdsHeaderName = "X-XRDS-Location"

-- |Get the (first) XRDS-Location header from the response
getXrdsLocationHeader :: [HTTP.Header] -> Maybe String
getXrdsLocationHeader headers =
    let customHeaders = [(map toLower k, v) |
                         HTTP.Header (HTTP.HdrCustom k) v <- headers]
        -- Regrettably, Eq on Header doesn't case fold custom header
        -- names
        toFind = map toLower xrdsHeaderName
    in lookup toFind customHeaders

-- |Get the XRDS Location URI from the response
getXrdsLocation :: (DiscoveryError e, MonadError e m) =>
                   Response.Response -> m (Maybe URI)
getXrdsLocation resp =
    let xrdsString = getXrdsLocationHeader (Response.headers resp) `mplus`
                     xrdsLocationFromHtml (Response.body resp)
    in case xrdsString of
         Nothing -> return Nothing
         Just locStr -> let badUri = badOpenIDMetadata locStr
                        in maybe badUri (return . Just) $ parseURI locStr

hasXrdsContentType :: [HTTP.Header] -> Bool
hasXrdsContentType =
    HTTP.lookupHeader HTTP.HdrContentType
    >>> maybe False (contentTypeMatches xrdsContentType)

contentTypeMatches :: String -> String -> Bool
contentTypeMatches expectedType = break (== ';')
                                  >>> fst
                                  >>> strip
                                  >>> map toLower
                                  >>> (== expectedType)

-- |Given the HTTP Response for the claimed identifier, return an XRDS
-- document or throw an error if we can't find an XRDS document
getXrdsXmlStr :: (DiscoveryError e, MonadFetch msg e m) =>
                 Response.Response -> m String
getXrdsXmlStr resp =
    if hasXrdsContentType $ Response.headers resp
    then do Trace.log "The document has the XRDS content-type"
            return $ Response.body resp
    else do xrdsLoc <- getXrdsLocation resp
            when (isNothing xrdsLoc) $
                 badOpenIDMetadata "No XRDS-Location found"
            let xrdsAcceptHeader = HTTP.Header HTTP.HdrAccept xrdsContentType
            httpGetContents (fromJust xrdsLoc, [xrdsAcceptHeader])
