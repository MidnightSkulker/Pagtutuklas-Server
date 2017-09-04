-- | Fields relating to OpenID in the database
module RelyingParty.Field
    ( Field(..)
    , name
    , field
    , names
    , nameMap
    , fieldMap
    ) where

import Data.Maybe ( fromJust )
import qualified Data.Map as Map

-- | Fields relating to OpenID in the database
data Field = NickName
           | EMail
           | FullName
           | DateOfBirth
           | Gender
           | PostCode
           | Country
           | Language
           | TimeZone
             deriving (Show, Ord, Eq, Bounded)

-- | Get the name from a field
name :: Field -> String
name f = fromJust $ Map.lookup f nameMap

-- | Access the map from strings to fields.
field :: String -> Field
field = (Map.!) fieldMap

-- | Fields and their printed names
names :: [(Field, String)]
names = [ (NickName, "nickname")
        , (EMail, "email")
        , (FullName, "fullname")
        , (DateOfBirth, "dob")
        , (Gender, "gender")
        , (PostCode, "postcode")
        , (Country, "country")
        , (Language, "language")
        , (TimeZone, "timezone")
        ]

-- | Build a map from fields to strings
nameMap :: Map.Map Field String
nameMap = Map.fromList names

-- | Build a map from a string to a field
fieldMap :: Map.Map String Field
fieldMap = Map.fromList $ map (\(a, b) -> (b, a)) names
