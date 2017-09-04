-- | Defines only the message priorities
module Config.Level ( Level (..) ) where

-- |Log message priorities. Higher levels are more likely to be logged
data Level = Debug | Verbose | Normal deriving (Eq, Show, Ord)
