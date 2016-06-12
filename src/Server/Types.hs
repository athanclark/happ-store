{-# LANGUAGE
    TemplateHaskell
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  #-}

module Server.Types where

import Data.Acid
import Data.SafeCopy
import Data.Data


-- | Basically a percentage, where %50 is identity
newtype Vote = Vote
  { getVote :: Float
  } deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Vote)

instance Show Vote where
  show (Vote p) = take 4 (show $ p * 100) ++ "%"

makeVote :: Float -> Maybe Vote
makeVote x
  | x > 1     = Nothing
  | x < 0     = Nothing
  | otherwise = Just (Vote x)

-- | Over average
instance Monoid Vote where
  mempty = Vote 0.5
  mappend (Vote x) (Vote y) =
    Vote $ (x + y) / 2
