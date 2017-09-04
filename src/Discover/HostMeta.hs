{-# LANGUAGE FlexibleContexts #-}
{-| Extract links to site XRDS files from a @host-meta@ file -}
module Discover.HostMeta
    ( siteXrdsUris
    , relXrdOp
    , hostMetaUri
    , relDescribedBy
    , hasType
    , isUriTemplate
    , uriTemplateFromLinkPatterns
    ) where

import Network.OpenID.JanRain.Common
import Network.URI ( URI(..), parseURI )
import Control.Arrow ( (>>>) )
import Data.Maybe ( listToMaybe )
import Text.HostMeta ( Link(..), LinkParam(..), LinkPattern(..), LinkRelationType(..) )

type ContentType = String

-- |Given a 'LinkPattern', returns whether it qualifies as a URI
-- template Link-Pattern header.
isUriTemplate :: LinkPattern -> Bool
isUriTemplate linkPattern = any (hasType xrdsContentType) params
                            where params = linkPatternParams linkPattern

-- |Given a list of 'LinkPattern's, find all URI templates in the list
-- and return the first URI template found, or Nothing.
uriTemplateFromLinkPatterns :: [LinkPattern] -> Maybe String
uriTemplateFromLinkPatterns linkPatterns =
    listToMaybe $ map uriTemplate uriTemplates
    where uriTemplates = filter isUriTemplate linkPatterns

-- |Given a list of Link elements from a parsed @host-meta@ file,
-- return a list of URIs to a site XRDS file
siteXrdsUris :: [Link] -> [URI]
siteXrdsUris = filter (linkParams >>> isSiteXrdsLink) >>> map linkUri

-- |Do these 'LinkParam's describe a site XRDS URI?
isSiteXrdsLink :: [LinkParam] -> Bool
isSiteXrdsLink params =
    and $ map (`any` params) $ [ hasType xrdsContentType
                               , hasRel relDescribedBy
                               , hasRel relXrdOp
                               ]

relXrdOpUri :: URI
Just relXrdOpUri = parseURI "http://specs.openid.net/auth/2.5/xrd-op"

-- |link relation for site-xrds URIs
relXrdOp :: LinkRelationType
relXrdOp = ExtRelationType relXrdOpUri

--------------------------------------------------
-- Should go in a generic HostMeta module

-- |Given a 'URI', return the URI to the corresponding @host-meta@ file
hostMetaUri :: URI -> URI
hostMetaUri uri = uri { uriPath = "/host-meta"
                      , uriQuery = ""
                      , uriFragment = "" }


--------------------------------------------------
-- Stuff that should go in a generic Link module

-- |Is this 'LinkParam' a @type@ param with the specified 'ContentType'?
hasType :: ContentType -> LinkParam -> Bool
hasType desiredContentType (LinkType ct) = ct == desiredContentType
hasType _ _ = False

-- |Is this 'LinkParam' a @rel@ with the specified 'LinkRelationType'?
hasRel :: LinkRelationType -> LinkParam -> Bool
hasRel desiredRel (LinkRel rels) = any (== desiredRel) rels
hasRel _ _ = False

-- |@describedby@ link relation
relDescribedBy :: LinkRelationType
relDescribedBy = RegRelationType "describedby"
