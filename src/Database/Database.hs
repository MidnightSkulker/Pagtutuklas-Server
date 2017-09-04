{-# OPTIONS -XScopedTypeVariables #-}
module Database.Database
    ( quickFetch
    , bsToString
    , strVal
    , WhereClause(..)
    ) where

-- Haskell / hackage
import Database.HDBC.PostgreSQL ( Connection )
import Database.HDBC ( SqlValue(..), quickQuery )
import Control.Monad.Trans ( liftIO )
import Data.ByteString ( ByteString, unpack )
import Data.Char ( chr )
import Application ( Application )

quickFetch :: String -> [SqlValue] -> Application st [[SqlValue]]
quickFetch query params = do
   conn :: Connection <- error ( " Not put in snap state yet " ) -- FIXME  
   -- Run the query
   liftIO $ quickQuery conn query params

-- | Convert and SQL value into a string, but not when the SQL value
--   is @SqlNull@
strVal :: SqlValue -> Maybe String
strVal (SqlByteString bs) = Just $ bsToString bs
strVal (SqlString s) = Just s
strVal SqlNull = Nothing
strVal unexpected = error $ "Got a non-string result: " ++ show unexpected

-- | Convert a bytestring to a string
bsToString :: ByteString -> String
bsToString = map (chr . fromIntegral) . unpack

-- | Data required to construct a where clause
data WhereClause = WhereClause { condition :: String
                               , args      :: [SqlValue]
                               } deriving ( Eq )


-- withConnection :: (Connection -> Application a) -> Application a
-- withConnection handler = do
  -- Get the connection from the Snap state
  --  pool <- asksAppState getDBConnPool
--  pool <- error ( " Not put in snap state yet " ) -- FIXME
  
--  withToIO $ \toIO -> Pool.withConnection pool $ toIO . handler

-- quickFetch :: (AppState st c, HasDBState st) =>
--               String -> [SqlValue] -> AppM st [[SqlValue]]
-- quickFetch query params =
--     withConnection $ \conn -> liftIO $ quickQuery conn query params

-- strVal :: SqlValue -> Maybe String
-- strVal (SqlByteString bs) = Just $ bsToString bs
-- strVal (SqlString s) = Just s
-- strVal SqlNull = Nothing
-- strVal unexpected = error $ "Got a non-string result: " ++ show unexpected
