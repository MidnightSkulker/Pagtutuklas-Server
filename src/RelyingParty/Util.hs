{-# OPTIONS -XExistentialQuantification -XScopedTypeVariables #-}
-- | Utilities for the BaseUrl server
module RelyingParty.Util
    ( -- Operations involving the database interface
      getRpByAppId  
      -- Operations involving the internal state 
    , getRequiredParam
      -- Supporting data types
    , ParameterError(..)
      -- Other utilities
    , urlToURI
    , getHTTPsURLParam
      -- Responses
    , errorResponse
    , okResponse
    , ok
    , notFound
    , found    
    ) where

import           Data.Maybe ( fromJust, isNothing )
import qualified Data.ByteString.Char8 as B
import           Control.Monad ( unless, when )
import qualified Snap.Types            as S
import qualified Text.JSONb            as J
import           Application ( Application )
import           RelyingParty.RelyingParty ( RelyingParty (..), loadByAppId )
import           Network.URL ( exportURL, URL (..), importURL, URLType (..) 
                             , Host (..), Protocol (..) )
import           Network.URI ( parseURI, URI )
import           Data.CIByteString ( toCI )

--------------------------------------------------------------------------------
-- Operations involving the database interface
--------------------------------------------------------------------------------

class Renderable a where
    toResponse :: a -> Application st S.Response

instance Renderable S.Response where
    toResponse = return

-- | Paramater errors handled by the server
data ParameterError = MissingParameter String
                    | InvalidParameter String
                    | DatabaseError String
                      
instance Show ParameterError where
    show (MissingParameter s) = "Missing argument: " ++ s
    show (InvalidParameter s) = "Invalid argument: " ++ s
    show (DatabaseError s)    = "Database Error: " ++ s
  
instance Renderable ParameterError where
    toResponse p@(MissingParameter _s) = errorResponse p "text/plain" 400
    toResponse p@(InvalidParameter _s) = errorResponse p "text/plain" 400
    toResponse p@(DatabaseError _s)    = errorResponse p "text/plain" 400

-- | Returns the RelyingParty by appId
--   The appId is from the HTTP request. The relying party is looked
--   up in the database
getRpByAppId :: Application st RelyingParty
getRpByAppId = do
  -- Get the application ID from the HTTP request (which is fished out
  -- of the Snap state)
  appId <- getRequiredParam (B.pack "appId")
  mRp <- loadByAppId (B.unpack appId)
  case mRp of
    Nothing -> errorResponse (InvalidParameter "appId: application not found")
                             "text/plain" 500
    Just rp -> return rp

--------------------------------------------------------------------------------
-- Operations involving the Snap internal state
--------------------------------------------------------------------------------

-- | Get a parameter from the HTTP request currently being processed.
--   @S.getParam@ is the snap routine for fishing this out of the
--   monad plumbing.
getRequiredParam :: B.ByteString -> Application st B.ByteString
getRequiredParam name = do
  -- Get the named parameter from the HTTP request.
  param <- S.getParam name
  case param of
    Nothing  -> errorResponse (MissingParameter (B.unpack name)) "text/plain" 400
    Just val -> return val            -- Found it, return

-- | Generate a response for a server internal error (500)
errorResponse :: ParameterError -> String -> Int -> Application st a
errorResponse err contentType status =
    do -- Encode response as JSON
       let msg' = B.concat [ B.pack "RPXNOW._base_cb(false, "
                           , J.encode J.Compact $ J.String (B.pack ( show err ))
                           , B.pack ");\n"
                           ]
       -- Tweak response, with HTTP status and content type,
       -- stuff the message in the response, and set content type.
       -- It is a little hard to see in the code, but these three
       -- actions are composed using @.@
       S.modifyResponse ( S.setResponseCode status .
                          S.setContentType ( B.pack contentType ) .
                          S.setContentLength (fromIntegral $ B.length msg')
                        )
       -- Write the modified message (@msg'@) into the Snap plumbing as  
       -- a response, and Snap will get around to actually sending the
       -- response out
       S.writeBS msg'
       -- @S.getResponse@ gets the response out of the snap state. It is
       -- then fed to @S.finishWith@, which completes the processing of
       -- the request.
       S.finishWith =<< S.getResponse

-- | Convert a URL to a URI
urlToURI :: URL -> URI
urlToURI = fromJust . parseURI . exportURL

-- | Get a URL parameter from the HTTP request
getHTTPsURLParam :: String -> Application st URL
getHTTPsURLParam name = do param <- getRequiredParam (B.pack name)
                           getHTTPsURLParamHelper name param

-- | Get the 
getHTTPsURLParamHelper :: String -> B.ByteString -> Application st URL
getHTTPsURLParamHelper name param = do
  let murl = importURL (B.unpack param)
  when ( isNothing murl ) $ errorResponse parseErr "text/plain" 400
  let url = fromJust murl
  unless (isAbsolute url) $ errorResponse absErr "text/plain" 400
  unless (isHTTPs url) $ errorResponse httpErr  "text/plain" 400
  return url
    where
        parseErr :: ParameterError = InvalidParameter $ name ++ " must be a URL"
        absErr = InvalidParameter $ name ++ " must be an absolute URL"
        httpErr = InvalidParameter $ name ++ " must be an HTTP(s) URL"
        isAbsolute u = case url_type u of
                           Absolute _ -> True
                           _ -> False
        isHTTPs u = case  url_type u of
                      Absolute (Host (HTTP _) _ _) -> True
                      _ -> False

-- getHTTPsURLParamOpt :: String -> ExceptionalT ParameterError (AppMIO st) (Maybe URL)
-- getHTTPsURLParamOpt name = do
--   param <- lift $ askQueryArg name
--   case param of
--       Nothing -> return Nothing
--       Just p -> do
--           url <- getHTTPsURLParamHelper name p
--           return $ Just url

-- | Create an "OK" HTTP response
--   The content is JSON encoded, so the content type is set to "application/JSON"
okResponse :: B.ByteString -> Application st ()
okResponse body =
      do let msg = B.concat [ B.pack "RPXNOW._base_cb(false, "
                            , J.encode J.Compact $ J.String (B.pack ( show body ))
                            , B.pack ");\n" ]
         S.modifyResponse ( S.setResponseCode 200 .
                            S.setContentType ( B.pack "application/json" ) .
                            S.setContentLength (fromIntegral $ B.length msg) )
         S.writeBS msg

-- | Create an "OK" HTTP response, with a null body.
ok :: Application st ()
ok = S.modifyResponse ( S.setResponseCode 200 .
                        S.setContentType ( B.pack "text/HTML" ) .
                        S.setContentLength 0 )

-- | Prepare a "not found" response
notFound :: B.ByteString -> Application st ()
notFound body = do
  let msg = B.concat [ B.pack "RPXNOW._base_cb(false, "
                     , J.encode J.Compact $ J.String (B.pack ( show body ))
                     , B.pack ");\n" ]
  S.modifyResponse ( S.setResponseStatus 404 ( B.pack "Found" ) .
                     S.setContentType ( B.pack "text/HTML" ) .
                     S.setContentLength (fromIntegral $ B.length msg) )
  S.writeBS msg
       
-- | Prepare a "found" response
found :: URI -> Application st ()
found dest =
  S.modifyResponse ( S.setResponseStatus 302 ( B.pack "Found" ) .
                     S.setContentType ( B.pack "text/HTML" ) .
                     S.setContentLength 0 .
                     S.addHeader ( toCI ( B.pack "Location" ) )
                                 ( B.pack ( show dest ) ) )
