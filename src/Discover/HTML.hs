module Discover.HTML
    ( xrdsLocationFromHtml
    , openIdLinkTags
    , EndpointInfo(..)
    , OpenIDVersion(..)
    ) where

import Network.OpenID.JanRain.Version

import Control.Applicative ( (<$>), (<|>) )
import Control.Arrow ( (>>>) )
import Data.Maybe ( maybeToList )
import Data.Char ( toLower, isSpace )
import Control.Monad ( guard, msum, mplus )
import Text.HTML.Tagchup.Parser ( runSoup )
import Text.HTML.Tagchup.Tag hiding (open, close)
import Text.HTML.Tagchup.Tag.Match
import Text.XML.Basic.Attribute ( toPair )
import Text.XML.Basic.Name ( match )
import qualified Text.XML.Basic.Name.LowerCase as LowerCaseName
import Network.URI

xrdsLocationFromHtml :: String -> Maybe String
xrdsLocationFromHtml = getHeadTags
                       >>> map getXrdsLocation
                       >>> msum

data EndpointInfo = EndpointInfo { version :: OpenIDVersion
                                 , endpointUri :: URI
                                 , localId :: (Maybe String)
                                 } deriving Show

type Tag = T LowerCaseName.T String

openIdLinkTags :: String -> [EndpointInfo]
openIdLinkTags = getHeadTags
                 >>> concatMap parseLinkTags
                 >>> collectEndpoints

getHeadTags :: String -> [Tag]
getHeadTags = runSoup
              >>> stripScriptTags
              >>> takeWhile isTagFromHead

parseLinkTags :: Tag -> [(String, String)]
parseLinkTags (Open name nameAttrs) | match "link" name = do
  let attrs = map toPair nameAttrs
  relStr <- maybeToList $ lookup "rel" attrs
  rel <- splitOnSpace relStr
  href <- maybeToList $ lookup "href" attrs
  return (rel, href)
parseLinkTags _ = []

splitOnSpace :: String -> [String]
splitOnSpace s =
    let stripped = dropWhile isSpace s
        (tok, rest) = break isSpace stripped
    in if null tok
       then []
       else tok:splitOnSpace rest

collectEndpoints :: [(String, String)] -> [EndpointInfo]
collectEndpoints linkInfo =
    collect "openid2.provider" "openid2.local_id" OpenID2 linkInfo
                `mplus`
    collect "openid.server" "openid.delegate" OpenID1 linkInfo

collect :: String -> String -> OpenIDVersion -> [(String, String)]
        -> [EndpointInfo]
collect sKey lKey ver tagInfo = do
  endpointUriStr <- [s | (relName, s) <- tagInfo, relName == sKey]
  endpointUri' <- maybeToList $ parseURI endpointUriStr
  let localIds = [s | (relName, s) <- tagInfo, relName == lKey]
      mkEndpoint = EndpointInfo ver endpointUri'
  if null localIds
    then return $ mkEndpoint Nothing
    else map (Just >>> mkEndpoint) localIds

getXrdsLocation :: Tag -> Maybe String
getXrdsLocation (Open name nameAttrs) | match "meta" name = do
  let attrs = map toPair nameAttrs
  httpEquiv <- lookup "http-equiv" attrs
  guard $ map toLower httpEquiv == "x-xrds-location"
  lookup "content" attrs
getXrdsLocation _ = Nothing

tagsBeforeBody :: [String]
tagsBeforeBody =
    ["html" , "head" , "base" , "link" , "meta" , "title" , "style"]

getTagName :: Tag -> Maybe (Name LowerCaseName.T)
getTagName tag = (fst <$> maybeOpen tag) <|> maybeClose tag

isTagFromHead :: Tag -> Bool
isTagFromHead tag = case getTagName tag of
                         Just name -> any (`match` name) tagsBeforeBody
                         Nothing -> True


isTagOpenName :: String -> Tag -> Bool
isTagOpenName nameToMatch = open (match nameToMatch) ignore

isTagCloseName :: String -> Tag -> Bool
isTagCloseName = close . match

stripScriptTags :: [Tag] -> [Tag]
stripScriptTags [] = []
stripScriptTags tags =
    let (before, rest) = break (isTagOpenName "script") tags
        afterScriptTag =
            case rest of
              [] -> []
              (_:atOpenTag) ->
                  drop 1 $ dropWhile (not . isTagCloseName "script") atOpenTag
    in before ++ stripScriptTags afterScriptTag
