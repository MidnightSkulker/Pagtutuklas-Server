module RelyingParty.RelyingParty
    ( RelyingParty(..)
    , RelyingPartyId
    , HostType(..)
    , ConfiguredProvider(..)
    , OAuthCredentials(..)
    , hostType
    , rpBaseUri
    , rpProtocol
    , canonicalPolicyUri
    , tokenUrlWhitelisted
    , tokenUrlAllowed
    , load
    , loadByApiKey
    , loadByAppId
    , loadByHostType
    , loadRequested
    , realm

    , deviceTokenUrl

    -- *Provider-specific utility functions

    , googleCredentials
    , yahooCredentials
    , mySpaceCredentials

    , hasMySpace
    , hasYahoo
    ) where

import qualified Snap.Types            as S
import qualified Data.ByteString.Char8 as B
import Data.CIByteString ( toCI )
import Data.Maybe ( catMaybes, fromJust, isJust, listToMaybe, fromMaybe )
import qualified Data.Set as Set
import Data.List ( intercalate, isPrefixOf )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( guard )
import Network.URI ( URI(..), parseURI, uriRegName, escapeURIString, isUnreserved )
import State.RPXState ( MonadRPX (..), HasRPXState )
import Application ( Application )

import RelyingParty.Field as Field

import Database.Database ( quickFetch, WhereClause(..), strVal )

import qualified RelyingParty.DomainPattern as DomainPattern

import Database.HDBC ( SqlValue(..) )

type ApiKey = String

type RelyingPartyId = Integer

 -- | Host as either a subdomain or domain
data HostType = Subdomain String | Domain String deriving Show

-- | Types relating to OAuth
type OAuthKey = String
type OAuthSecret = String
-- | Open Auth Credentials
data OAuthCredentials =
  OAuthCredentials { oauthKey    :: OAuthKey
                   , oauthSecret :: OAuthSecret
                   } deriving (Eq, Show)

data ConfiguredProvider = MySpace OAuthCredentials
                        | Google OAuthCredentials [URI]
                        | Yahoo OAuthCredentials
                        | Facebook deriving (Eq, Show)

-- | Relying party information in the database.
data RelyingParty = RelyingParty
    { rpId                  :: RelyingPartyId
    , rpHost                :: HostType
    , rpDisplayName         :: Maybe String
    , rpConfiguredProviders :: [ConfiguredProvider]
    , rpTokenDomains        :: [DomainPattern.DomainPattern]
    , rpCustomRealm         :: Maybe String
    , rpHasFavicon          :: Bool
    , rpExternalId          :: String
    , rpPolicyUri           :: Maybe URI
    , rpSregFields          :: Set.Set Field.Field
    }

-- | Special case providers
data OAuthProvider = OYahoo | OTwitter | OMySpace | OGoogle

-- | Convert a provider into a string
widgetName :: OAuthProvider -> String
widgetName OYahoo   = "yahoo"
widgetName OTwitter = "twitter"
widgetName OMySpace = "myspace"
widgetName OGoogle  = "google"

-- | Determine if a relying party has mySpace credentials
hasMySpace :: RelyingParty -> Bool
hasMySpace = isJust . mySpaceCredentials

-- | Get the mySpace credentials from the relying party
mySpaceCredentials :: RelyingParty -> Maybe OAuthCredentials
mySpaceCredentials rp = listToMaybe [ c | MySpace c <- rpConfiguredProviders rp ]

-- | Determine if a relying party has Yahoo credentials
hasYahoo :: RelyingParty -> Bool
hasYahoo = isJust . yahooCredentials

-- | Get the yahoo credentials from the relying party
yahooCredentials :: RelyingParty -> Maybe OAuthCredentials
yahooCredentials rp = listToMaybe [ c | Yahoo c <- rpConfiguredProviders rp ]

-- | Get the Google credentials from the relying party
googleCredentials :: RelyingParty -> Maybe (OAuthCredentials, [URI])
googleCredentials rp =
    listToMaybe [ (c, us) | Google c us <- rpConfiguredProviders rp ]

-- | Get the realm from a relying party
realm :: (HasRPXState st) => RelyingParty -> Application st String
realm rp = maybe standardRealm return $ rpCustomRealm rp
  where standardRealm = show <$> rpBaseUri rp


canonicalPolicyUri :: (HasRPXState st) =>
                      RelyingParty -> Application st (Maybe URI)
canonicalPolicyUri rp = do
  case rpPolicyUri rp of
    Nothing -> return Nothing
    Just _ -> do
      base <- rpBaseUri rp
      return $ Just $ base { uriPath = "/openid/sreg_policy" }

-- | Get the protocol from the relying party
rpProtocol :: (HasRPXState st) => RelyingParty -> Application st String
rpProtocol rp = do
  case rpHost rp of
    Subdomain _ -> fishDefaultProto
    Domain _ -> do secureFeature <- hasFeature "secure_domain" rp
                   return $ if secureFeature then "https" else "http"

-- | Get the base URI from the relying party
rpBaseUri :: (HasRPXState st) => RelyingParty -> Application st URI
rpBaseUri rp = do
  protoStr <- rpProtocol rp
  -- 
  authority <- case rpHost rp of
    Subdomain r -> do base <- fishRPXHost -- *** Do we need to format RPX host?
                      return $ r ++ ('.':base)
    Domain domain -> return domain

  let uriStr = protoStr ++ "://" ++ authority ++ "/"
  case parseURI uriStr of
    Just uri -> return uri
    Nothing -> error $ "<rpBaseUri> Unable to parse expected uri: " ++ show uriStr

-- | Get the base host type from the relying party
hostType :: (HasRPXState st) => Application st (Maybe HostType)
hostType = do
  rpxHostStr <- fishRPXHost
  let getHostType :: B.ByteString -> HostType
      getHostType hostPort =
        let host = takeWhile (/= ':') (B.unpack hostPort)
            theEnd = reverse $ '.':rpxHostStr
            (hostEnd, hostStart) = splitAt (length theEnd) $ reverse (B.unpack hostPort)
        in if hostEnd == theEnd
           then Subdomain $ reverse hostStart
           else Domain host
  req <- S.getRequest
  let mHostPort = S.getHeader (toCI (B.pack "HdrHost")) req
  return $ fmap getHostType mHostPort

-- | Load the relying party for the given API key
loadByApiKey :: ApiKey -> Application st (Maybe RelyingParty)
loadByApiKey apiKey = load $ WhereClause "secret = ?" [SqlString apiKey]

-- | Load the relying party for the given Application ID
loadByAppId :: String -> Application st (Maybe RelyingParty)
loadByAppId appId = load $ WhereClause "appId = ?" [SqlString appId]

-- | Load the relying party for the given host type
loadByHostType :: HostType -> Application st (Maybe RelyingParty)
loadByHostType ht =
  load $ case ht of
    Domain d    -> WhereClause "domain = ?" [SqlString d]
    Subdomain s -> WhereClause "realm = ?" [SqlString s]

-- |Load the relying party that matches the currently requested host
-- (if any)
loadRequested :: (HasRPXState st) => Application st (Maybe RelyingParty)
loadRequested = maybe (return Nothing) loadByHostType =<< hostType

-- |The test tool token URL for this relying party, using the same
-- request protocol as the current request
testToolTokenUrl :: (HasRPXState st) => RelyingParty -> Application st URI
testToolTokenUrl rp = do
  host <- fishRPXHost
  protoStr <- fishDefaultProto
  let urlStr = protoStr ++ "://" ++ host ++ path
      path = "/relying_parties/" ++ rpExternalId rp ++ "/test"
  return $ fromJust $ parseURI urlStr

-- |The social publish finish token URL for this relying party, using
-- the same request protocol as the current request
localTokenUrls :: (HasRPXState st) =>
                  RelyingParty -> Application st (URI, URI)
localTokenUrls rp = do
  baseUri <- rpBaseUri rp
  let path = "/"
      http = baseUri  { uriScheme = "http:",  uriPath = path }
      https = baseUri { uriScheme = "https:", uriPath = path }
  return (http, https)

-- |The device finish token URL for this relying party, using
-- the same request protocol as the current request
deviceTokenUrl :: (HasRPXState st) =>
                  RelyingParty -> String -> Application st URI
deviceTokenUrl rp device = do
  baseUri <- rpBaseUri rp
  let escapedDev = escapeURIString isUnreserved device
  return $ baseUri { uriPath = "/signin/device"
                   , uriQuery = "?device=" ++ escapedDev }

-- |Low-level function to check a token URL against the token URL
-- domain whitelist for the specified relying party.  See
-- tokenUrlAllowed.
tokenUrlWhitelisted :: RelyingParty -> URI -> Bool
tokenUrlWhitelisted rp tokenUrl = isJust authority && or matches
    where
      matches = map (DomainPattern.match domain) patterns
      patterns = rpTokenDomains rp
      authority = uriAuthority tokenUrl
      domain = uriRegName $ fromJust authority

-- |Returns whether a token URL is permitted for the specified relying
-- party.  Checks the relying party's token URL domain whitelist and
-- specifically allows the test tool token URL for this relying party.
tokenUrlAllowed :: (HasRPXState st) => RelyingParty -> URI -> Application st Bool
-- XXX: don't check token_url for kickapps
tokenUrlAllowed rp _ | rpId rp == 2620 = return True

tokenUrlAllowed rp tokenUrl = do
  testToolUri <- testToolTokenUrl rp
  (socialUri1, socialUri2) <- localTokenUrls rp
  deviceUri <- deviceTokenUrl rp ""
  let tokenUrlStr = show tokenUrl
  return $ (tokenUrlWhitelisted rp tokenUrl ||
            testToolUri == tokenUrl ||
            show socialUri1 `isPrefixOf` tokenUrlStr ||
            show socialUri2 `isPrefixOf` tokenUrlStr ||
            show deviceUri `isPrefixOf` tokenUrlStr)

sregFieldSelect :: String
sregFieldSelect = intercalate ", " $ map (("sreg_" ++) . snd) Field.names

extractSregFields :: [SqlValue] -> Set.Set Field.Field
extractSregFields vals = Set.fromList [f | (b, f) <- pairedFields, b]
    where unSqlBool (SqlBool b) = b
          unSqlBool v = error $ "Unexpected value for sreg field: " ++ show v
          pairedFields = zip (map unSqlBool vals) (map fst Field.names)

-- | Send a query to the database to determine if the specified feature
--   is present
hasFeature :: String -> RelyingParty -> Application st Bool
hasFeature feature rp = do
  let query = "SELECT rp_has_feature(?, ?)"
  rows <- quickFetch query [SqlInteger $ rpId rp, SqlString feature]
  case rows of
    [[SqlBool result]] -> return result
    x -> fail $ "Unexpected hasFeature result for " ++ show (rpId rp, feature)
         ++ ": returned " ++ show x

-- | Get the Google credentials from the relying party DB
loadGoogleCredentials :: RelyingPartyId -> Application st (Maybe (OAuthCredentials, [URI]))
loadGoogleCredentials theRpId = do
  uris <- loadUris
  creds <- loadOAuthCredentials theRpId OGoogle
  return $ (,) <$> creds <*> uris
    where
      -- Load the URIs from the relying party database
      loadUris = do
        -- Format the query as a SELECT
        let query = "SELECT url \
                    \FROM oauth_scopes s \
                    \  JOIN relying_party_google_scopes g \
                    \  ON s.id = g.oauth_scope_id \
                    \WHERE relying_party_id = ?"
        -- Perform the query, returning some rows from the DB
        rows <- quickFetch query [SqlInteger theRpId]

        let parseScope :: String -> URI
            parseScope uriStr = fromMaybe err $ parseURI uriStr
                where err = error $ "Scope failed to parse: " ++ show uriStr

            convertRow [s] = parseScope <$> strVal s
            convertRow r = error $ "Unexpected row: " ++ show r

            uris = catMaybes $ map convertRow rows

        return $ guard (not $ null uris) >> return uris


-- | Present a SELECT SQL statement to the relying party data base
--   asking for OAuth credentials
loadOAuthCredentials :: RelyingPartyId -> OAuthProvider ->
                        Application st (Maybe OAuthCredentials)
loadOAuthCredentials theRpId prov = do
  { let -- Format the query to the relying party database
        query = "SELECT key, secret \
                \FROM oauth_provider_keys pk JOIN widget_providers wp \
                \  ON pk.widget_provider_id = wp.id \
                \WHERE relying_party_id = ? AND wp.widget_name = ?"
        widgetN = widgetName prov
  ; tuples <- quickFetch query [SqlInteger theRpId, SqlString widgetN]
  ; return ( sqlCredsToOauthCreds tuples )
  } where sqlCredsToOauthCreds :: [[SqlValue]] -> Maybe OAuthCredentials
          sqlCredsToOauthCreds result =
            case map (map strVal) result of
               -- Should return just one result
               [[Just k, Just s]] -> Just $ OAuthCredentials k s
               [] -> Nothing
               -- Error if more than one result
               _ -> error $ "Unexpected result: " ++ show result

-- | Present a SELECT SQL statement to the relying party database.
load :: WhereClause -> Application st (Maybe RelyingParty)
load w = do
  let -- Define the database query
      query = "SELECT rps.id, rps.custom_realm, rps.realm, rps.domain,  \
              \  rps.display_name, (rps.favicon_id IS NOT NULL) AS has_favicon,  \
              \  rps.external_id, rp_has_feature(rps.id, 'facebook_auth') AS facebook_allowed,  \
              \  fa.api_key AS facebook_key, fa.secret AS facebook_secret,  \
              \  rps.sreg_policy_url, " ++ sregFieldSelect ++ "\
              \  FROM (SELECT * FROM relying_parties WHERE " ++ condition w ++ ") rps  \
              \  LEFT JOIN facebook_auth_infos fa ON fa.relying_party_id = rps.id  \
              \  LEFT JOIN liveids l ON rps.id = l.relying_party_id"
  -- Do the fetch of the matching tuples (if any)
  result <- quickFetch query $ args w
  -- Analyze the result of the DB query
  case result of
    -- When nothing was returned, so indicate
    [] -> return Nothing
    -- When several were returned, the first should match this very
    -- elaborate syntax.
    [SqlInteger i:  -- @i@ is the relying party ID
     customRlmVal:
     rlmVal:
     domVal:
     displayVal:
     SqlBool hasFavicon:
     externalIdVal:
     SqlBool fbAllowed:
     fbKey:
     fbSecret:
     sregPolicyVal:
     sregFieldVals] -> do
        -- Construct a relying party return result
        let display = strVal displayVal
            customRealm = strVal customRlmVal
            policyUrl = strVal sregPolicyVal >>= parseURI
            sregFields = extractSregFields sregFieldVals
        -- Check for null external ID Val
        externalId <- case strVal externalIdVal of
                        Nothing -> fail "Expected an external_id, got NULL"
                        Just v -> return v
        -- Determine if domain or subdomain
        ht <- case (strVal rlmVal, strVal domVal) of
                (Nothing, Just d) -> return $ Domain d
                (Just s, Nothing) -> return $ Subdomain s
                _ -> fail $ "Expected exactly one of realm or domain. Got: "
                            ++ show (rlmVal, domVal)
        -- Determine if a facebook relying party
        fb_enabled <- case (strVal fbKey, strVal fbSecret) of
          (Nothing, Nothing) -> return Nothing
          (Just _, Just _) -> if fbAllowed
                              then return $ Just Facebook
                              else return Nothing
          _ -> fail $ "Expected none or both of facebook secret and key"
        -- Determine if MySpace enabled
        my_enabled <- fmap MySpace <$> loadOAuthCredentials i OMySpace
        -- Determine if Google enabled
        g_enabled <- fmap (uncurry Google) <$> loadGoogleCredentials i
        -- Determine if Yahoo enabled
        y_enabled <- fmap Yahoo <$> loadOAuthCredentials i OYahoo
        -- List the providers
        let providers = catMaybes [ fb_enabled
                                  , my_enabled
                                  , g_enabled
                                  , y_enabled
                                  ]
        -- Get the domain patterns
        patterns <- DomainPattern.loadMany $ WhereClause "relying_party_id = ?" [SqlInteger i]
        -- Put it all together as a @RelyingParty@
        return $ Just $ RelyingParty { rpId = i
                                     , rpHost = ht
                                     , rpDisplayName = display
                                     , rpConfiguredProviders = providers
                                     , rpTokenDomains = patterns
                                     , rpCustomRealm = customRealm
                                     , rpHasFavicon = hasFavicon
                                     , rpExternalId = externalId
                                     , rpPolicyUri = policyUrl
                                     , rpSregFields = sregFields
                                     }
    -- Too many rows returned from database
    _ -> fail $ "Expected one row when loading RP. Got: " ++ show result
