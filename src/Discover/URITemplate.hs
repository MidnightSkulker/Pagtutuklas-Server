module Discover.URITemplate ( fillURITemplate ) where

import Control.Monad.Error ( MonadError )
import Network.URI ( parseURI, URI(..), URIAuth(..), escapeURIString, isUnreserved )
import Control.Applicative ( (<$>) )
import Text.ParserCombinators.Parsec


data URIComponent = FullURI | Scheme | Authority | Path | Query
                  | Fragment | UserInfo | Host | Port deriving Show

type ShouldEncode = Bool

data TemplatePart = TemplateLiteral String
                  | TemplateReplace ShouldEncode URIComponent deriving Show


fillURITemplate :: MonadError e m => String -> URI -> m URI
fillURITemplate template uri = do
  parts <- parseURITemplate template
  let filled = concatMap (subTemplatePart uri) parts
  case parseURI filled of
    Nothing -> fail $ "failed to parse result of fill URI template: " ++ filled
    Just parsed -> return parsed

parseURITemplate :: MonadError e m => String -> m [TemplatePart]
parseURITemplate = either (fail . show) return .
                   parse parseTemplate "<internal>"

parseTemplate :: Parser [TemplatePart]
parseTemplate = do
  parts <- many $ ((try toReplace) <|> literal)
  eof
  return parts

toReplace :: Parser TemplatePart
toReplace = do
  char '{'
  shouldEncode <- option False (char '%' >> return True)
  comp <- component
  char '}'
  return $ TemplateReplace shouldEncode (tokenToURIComponent comp)


component :: Parser String
component = choice $ map (try . string) ["uri", "scheme", "authority",
                                         "path", "query", "fragment",
                                         "userinfo", "host"] ++ [string "port"]


literal :: Parser TemplatePart
literal = TemplateLiteral <$> (many1 $ noneOf "{")

tokenToURIComponent :: String -> URIComponent
tokenToURIComponent "uri" = FullURI
tokenToURIComponent "scheme" = Scheme
tokenToURIComponent "authority" = Authority
tokenToURIComponent "path" = Path
tokenToURIComponent "query" = Query
tokenToURIComponent "fragment" = Fragment
tokenToURIComponent "userinfo" = UserInfo
tokenToURIComponent "host" = Host
tokenToURIComponent "port" = Port
tokenToURIComponent _ = error "wha' happened?"


subTemplatePart :: URI -> TemplatePart -> String
subTemplatePart _ (TemplateLiteral s) = s
subTemplatePart uri (TemplateReplace enc comp) =
    (if enc then escapeURIString isUnreserved else id) $ subComponent comp uri


subComponent :: URIComponent -> URI -> String
subComponent FullURI = \uri -> uriScheme uri ++ "//" ++
                       maybe "" showAuthority (uriAuthority uri) ++
                       uriPath uri ++ uriQuery uri
subComponent Scheme = dropLast . uriScheme
subComponent Authority = (maybe "" showAuthority) . uriAuthority
subComponent Path = uriPath
subComponent Query = drop 1 . uriQuery
subComponent Fragment = drop 1 . uriFragment
subComponent UserInfo = (maybe "" (dropLast . uriUserInfo)) . uriAuthority
subComponent Host = (maybe "" uriRegName) . uriAuthority
subComponent Port = (maybe "" showAuthorityPort) . uriAuthority


showAuthority :: URIAuth -> String
showAuthority auth = (uriUserInfo auth) ++ (uriRegName auth) ++ (uriPort auth)

showAuthorityPort :: URIAuth -> String
showAuthorityPort auth = case uriPort auth of
                           (':':port) -> port
                           _ -> ""



dropLast :: [a] -> [a]
dropLast = reverse . drop 1 . reverse






