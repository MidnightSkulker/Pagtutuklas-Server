module Discover.WhiteSpace
       ( strip
       , lstrip
       , rstrip
       ) where

-- | Removes any whitespace characters that are present at the start
--   or end of a string. Does not alter the internal contents of a
--   string. If no whitespace characters are present at the start or end
--   of a string, returns the original string unmodified. Safe to use on
--   any string.
--
-- Note that this may differ from some other similar
-- functions from other authors in that:
--
-- 1. If multiple whitespace characters are present all in a row, they
--    are all removed;
-- 2. If no whitespace characters are present, nothing is done.
strip :: String -> String
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- | Things that are considered white space characters
wschars :: String
wschars = " \t\r\n"
