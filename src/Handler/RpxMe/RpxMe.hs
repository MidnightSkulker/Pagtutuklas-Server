{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.RpxMe.RpxMe
       ( rpxMe
       , pathPrefix
       , rollbackPool
       ) where

-- Haskell and hackage
import Data.Word ( Word8, Word64 )
import Data.Int ( Int64 )
import Data.Maybe ( catMaybes )
import Data.List ( elemIndex, intercalate )
import Data.Bits ( shiftL, (.|.) )
import Data.Time.Format ( formatTime )
import Data.Time.Clock ( getCurrentTime )
import Data.ByteString.UTF8 ( toString )
import Database.HDBC ( SqlValue(..) )

import Control.Monad.Trans ( liftIO )
import Network.URI ( URI, parseURI )
import System.Locale ( defaultTimeLocale )
import qualified Snap.Types as S
import qualified Data.ByteString.Char8 as B
import Text.JSON.Generic ( encodeJSON )
-- Janrain
import State.DBState ( MonadDB (..), HasDBState )
import State.BeanstalkState ( HasBeanstalkState )
import Database.Database ( quickFetch )
import qualified Database.HDBC.ConnectionPool as CONNPOOL
import Network.Beanstalk.Ext ( withBeanstalkServerOrFile )
import qualified RelyingParty.UrlRef as UrlRef
import RelyingParty.Util ( ok, found, notFound )
import Application ( Application )
import AppActions ( getRemoteAddrStr )

pathPrefix :: String
pathPrefix = "/rpxme/"

notFoundMsg :: B.ByteString
notFoundMsg = B.pack "<html><body>The URL you clicked on was not found</body></html>"

rpxMe :: ( HasBeanstalkState st, HasDBState st ) => Application st ()
rpxMe = do
  -- Need isPrefix stuff here????
  -- Get the HTTP requests from the Snap plumbing, and then get the headers
  requ <- S.getRequest
  let getPath = error ( "getPath" )
      path = getPath requ
      encodedId = drop ( length pathPrefix ) path
  let urlRefId = decodeUrlRefId encodedId

  -- Log to the analytics database (through beanstalkd).
  time <- liftIO $ currentTimestamp
  reqst <- S.getRequest
  let userAgentB :: Maybe B.ByteString = S.getHeader "HdrUserAgent" reqst
      userAgentS = case userAgentB of
                     Nothing -> Nothing
                     Just ua -> Just ( B.unpack ua )
  ip <- getRemoteAddrStr
  mUref <- if urlRefId <= fromIntegral (maxBound :: Int64)
           then UrlRef.loadOne urlRefId
           else return Nothing

  case mUref of
      Just uref ->
          (withBeanstalkServerOrFile json)
              where
                  pairToJson :: ( String, String ) -> String
                  pairToJson ( f, s ) = encodeJSON f ++ ":" ++ s
                  pairs :: [ ( String, String ) ]
                  pairs = maybeAddPairQ ("raw_user_agent", userAgentS) $
                          maybeAddPairQ ("ip_address", maybeByteString ip) $
                          maybeAddPairQ ("source_url", UrlRef.xdReceiverUrl uref) $
                          maybeAddPairQ ("provider_name", UrlRef.provider uref) $
                          [ ("relying_party_id", show $ UrlRef.relyingPartyId uref)
                          , ("identifier_id",    show $ UrlRef.identifierId uref  )
                          , ("url",              encodeJSON $ UrlRef.url uref )
                          , ("created_at",       encodeJSON time )
                          , ("device",           encodeJSON ("web" :: String) )
                          ]
                  maybeAddPairQ :: (String, Maybe String) -> [(String, String)] -> [(String, String)]
                  maybeAddPairQ (key, value) values =
                      case value of
                          Just str -> (key, encodeJSON str) : values
                          _ -> values
                  json = let front :: String = "{\"record_type\": \"engage_url_ref\", \"attributes\": {"    
                             middle :: String = intercalate "," (map pairToJson pairs) 
                             end :: String = "}}\n"
                         in front ++ middle ++ end

      Nothing -> return ()

  -- Also record the URL referral in the logins table for now (to be
  -- removed).
  mUri <- if urlRefId <= fromIntegral (maxBound :: Int64)
          then lookupUrl urlRefId
          else return Nothing

  case mUri of
     Nothing -> notFound notFoundMsg
     Just uri -> found uri


decodeUrlRefId :: String -> Integer
decodeUrlRefId = fromIntegral . accumHelper . toBytes
    where toBytes :: String -> [Word8]
          toBytes xs = reverse $
                       map fromIntegral $
                       catMaybes [elemIndex x space | x <- xs]

          accumBytes :: Word64 -> [Word8] -> Word64
          accumBytes accum [] = accum
          accumBytes accum (x:xs) = let i = shiftL accum 6
                                        j = (fromIntegral x) .|. i
                                    in accumBytes j xs

          accumHelper = accumBytes 0

          space = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"


-- lookup the url while incrementing the click counter in url_refs
lookupUrl :: Integer -> Application st (Maybe URI)
lookupUrl urlRefId = do
  result <- quickFetch query $ [SqlInteger urlRefId]
  case result of
    [[SqlNull]] -> return Nothing
    [[SqlByteString url]] -> return $ do uri <- parseURI $ toString url
                                         return uri
    _ -> fail $ "Expected one row when loading URL. Got: " ++ show result

query :: String
query = "SELECT update_and_fetch_url(?)"

currentTimestamp :: IO String
currentTimestamp = do
  utc <- getCurrentTime
  return $ formatTime defaultTimeLocale "%F %T%Q %Z" utc

rollbackPool :: ( HasDBState st ) => Application st ()
rollbackPool = do
    pool <- fishConnPool
    liftIO $ CONNPOOL.rollbackPool pool
    ok

maybeByteString :: Maybe B.ByteString -> Maybe String
maybeByteString b = fmap B.unpack b
