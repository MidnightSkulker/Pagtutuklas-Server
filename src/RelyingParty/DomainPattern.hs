module RelyingParty.DomainPattern
    ( loadMany
    , match
    , DomainPattern ( .. )
    , mkSimplePattern
    , mkWildCardPattern
    ) where

-- Haskell and hackage
import Database.HDBC ( SqlValue(..) )
import Data.List ( isSuffixOf )
-- Local imports
import Database.Database ( quickFetch, WhereClause(args,condition), bsToString )
import Application ( Application )

data DomainPattern = DomainPattern { wildCard :: Bool, pattern :: String }

-- | Make a pattern without wildcards
mkSimplePattern :: String -> DomainPattern
mkSimplePattern s = DomainPattern { wildCard = False
                                  , pattern  = s
                                  }

-- | Make a pattern with wild cards
mkWildCardPattern :: String -> DomainPattern
mkWildCardPattern s = DomainPattern { wildCard = True
                                    , pattern  = s
                                    }

-- | Match a pattern against a domain pattern
--   Isn't there a library for this????
match :: String -> DomainPattern -> Bool
match domain p = (wildCard p && (not $ null $ drop (length (pattern p) + 1) domain)
                               && ('.':pattern p) `isSuffixOf` domain)
                 || domain == pattern p

-- | Query the data base when many matching tuples are expected.
loadMany :: WhereClause -> Application st [DomainPattern]
loadMany wc = do
  -- Build a query to the database
  let query = "SELECT wildcard, domain FROM rp_token_domain_patterns r \
              \ JOIN domain_patterns dp ON dp.id = r.domain_pattern_id \
              \WHERE " ++ condition wc
  -- Send the query to the database
  results <- quickFetch query $ args wc
  case results of
    -- No results, so return null list
    [] -> return []
    -- At least one result of the expected form, reformat the results as
    -- a list of domain patterns and return that list.
    ([SqlBool _, SqlByteString _]:_) ->
        return $ map (\[SqlBool w, SqlByteString p] -> DomainPattern w $ bsToString p) results
        where
    -- Otherwise we got some unexpected results.
    _ -> fail $ "Expected list of (bool, string) when loading domain patterns. Got: " ++ show results