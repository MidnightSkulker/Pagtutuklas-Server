{-# LANGUAGE FlexibleContexts #-}
module Discover.XRDS
    ( handleXrdsContentWith
    , extractUserEndpoints
    , extractUserEndpointsXRI
    , extractOpEndpoints
    , extractSiteXrdsOpEndpoints
    , extractUriTemplate
    ) where

import Network.URI ( URI, parseURI, uriAuthority, uriRegName )

import Control.Monad ( guard, when, msum, unless )
import Control.Monad.Error ( MonadError )

import Data.Maybe ( catMaybes, maybeToList )
import Data.List ( sortBy )
import Data.Function ( on )

import Fetcher.Errors
import qualified Network.OpenID.JanRain.Endpoint as E
import qualified Network.OpenID.JanRain.ParseXrds as ParseXrds

import Network.OpenID.JanRain.ParseXrds (XrdElement(canonicalID))

import qualified Network.OpenID.JanRain.Identifier as Id
import Network.OpenID.JanRain.ParseXrds ( ServiceElement )

import Network.OpenID.JanRain.Version
    ( openId1TypeUris, openId2Type, openId2IDPType )

-- |Parse an XRDS document into a list of endpoints or an error message
handleXrdsContentWith ::
    (DiscoveryError e, MonadError e m) =>
         (ParseXrds.XrdElement -> m [E.Endpoint]) -> String -> m [E.Endpoint]
handleXrdsContentWith extractEndpoints xrdsStr = do
  xrd <- parseXrds xrdsStr
  endpoints <- extractEndpoints xrd
  when (null endpoints) $ notAnOpenID "No endpoints found in XML"
  return endpoints

parseXrds :: (DiscoveryError e, MonadError e m) =>
             String -> m ParseXrds.XrdElement
parseXrds = either badOpenIDMetadata return . ParseXrds.parse

processXrd :: ParseXrds.XrdElement -> (ServiceElement -> [a]) -> [a]
processXrd xrd extractEndpoints =
  let serviceElements = sortBy comparePriority $ ParseXrds.serviceElements xrd
  in extractEndpoints =<< serviceElements

extractUriTemplate :: (DiscoveryError e, MonadError e m) =>
                      URI -> String -> m (Maybe String)
extractUriTemplate originalUri content = do
  xrd <- parseXrds content
  checkSiteXrdsCanonicalId originalUri xrd
  return $ msum $ map ParseXrds.uriTemplate $ ParseXrds.serviceElements xrd

extractUserEndpoints :: Monad m => URI -> ParseXrds.XrdElement -> m [E.Endpoint]
extractUserEndpoints claimedIdUri xrd =
    return $ processXrd xrd $ \element -> do
      let ident = Id.IdURI claimedIdUri
      localId <- maybeToList $ getLocalId ident element
      makeEndpoints element $ Just $ uriEndUserInfo ident localId

extractUserEndpointsXRI :: Monad m => String -> ParseXrds.XrdElement
                        -> m [E.Endpoint]
extractUserEndpointsXRI displayXri xrd =
    return $ processXrd xrd $ \element -> do
      canon <- maybeToList $ canonicalID xrd >>= Id.parse
      localId <- maybeToList $ getLocalId canon element
      makeEndpoints element $ Just $ E.EndUserInfo canon localId displayXri

extractOpEndpoints :: Monad m => ParseXrds.XrdElement -> m [E.Endpoint]
extractOpEndpoints xrd =
    return $ processXrd xrd $ \element -> do
      guard $ isOpenID2IDP element
      makeEndpoints element Nothing

checkSiteXrdsCanonicalId :: (DiscoveryError e, MonadError e m) =>
                            URI -> ParseXrds.XrdElement -> m ()
checkSiteXrdsCanonicalId originalUri xrd = do
  cid <- case ParseXrds.canonicalID xrd of
           Nothing -> badOpenIDMetadata $
                      "no CanonicalID in site xrds for " ++ show originalUri
           Just i -> return i

  domain <- case uriAuthority originalUri of
              Nothing -> fail $ show originalUri ++ " doesn't have an authority"
              Just authority -> return $ uriRegName authority

  unless (cid == domain) $ badOpenIDMetadata $
             "site xrds CanonicalId doesn't match domain for " ++
             show originalUri

extractSiteXrdsOpEndpoints :: (DiscoveryError e, MonadError e m) =>
                              URI -> ParseXrds.XrdElement -> m [E.Endpoint]
extractSiteXrdsOpEndpoints originalUri xrd = do
  checkSiteXrdsCanonicalId originalUri xrd
  extractOpEndpoints xrd

toURIList :: [String] -> [URI]
toURIList = catMaybes . map parseURI

hasType :: ServiceElement -> URI -> Bool
hasType element typeUri = typeUri `elem` toURIList (ParseXrds.types element)

isOpenID1 :: ServiceElement -> Bool
isOpenID1 element = any (element `hasType`) openId1TypeUris

isOpenID2 :: ServiceElement -> Bool
isOpenID2 = (`hasType` openId2Type)

isOpenID2IDP :: ServiceElement -> Bool
isOpenID2IDP = (`hasType` openId2IDPType)

isUserOpenID :: ServiceElement -> Bool
isUserOpenID element = isOpenID2 element || isOpenID1 element

-- |If this service element represents a user OpenID, return the local ID
getLocalId :: Id.Identifier -> ServiceElement -> Maybe String
getLocalId ident element = msum [ isOpenID2 ~~> ParseXrds.localId element
                                , isOpenID1 ~~> ParseXrds.openIDDelegate element
                                , isUserOpenID ~~> return (show ident)
                              ]
    where cond ~~> val = guard (cond element) >> val

uriEndUserInfo :: Id.Identifier -> String -> E.EndUserInfo
uriEndUserInfo ident localId =
    E.EndUserInfo { E.euiClaimedId = ident
                  , E.euiLocalId = localId
                  , E.euiDisplayIdentifier = show ident
                  }


makeEndpoints :: ServiceElement -> Maybe E.EndUserInfo -> [E.Endpoint]
makeEndpoints element endUserInfo = do
    let prioritized = sortBy (compare `on` snd) $ ParseXrds.uris element
    serverUri <- toURIList $ map fst prioritized
    return $ E.Endpoint { E.endUserInfo = endUserInfo
                          , E.serverEndpoint = serverUri
                          , E.typeUris = toURIList $ ParseXrds.types element
                          , E.usedYadis = True
                          }

comparePriority :: ServiceElement -> ServiceElement -> Ordering
comparePriority = compare `on` ParseXrds.priority
