{-# LANGUAGE
    TemplateHaskell
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  #-}

module Server.Types where

{-| Data that only deals with voting and reviews -}

import Schema.Types
import Cabal.Types

import Data.SafeCopy
import Data.Data
import Data.IxSet
import Data.Hashable
import Data.Time
import qualified Data.Text.Lazy as LT
import qualified Data.HashMap.Strict as HMS
import Control.Monad



-- * Votes

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

-- * Identifiers

newtype ReviewId = ReviewId
  { getReviewId :: Integer
  } deriving (Show, Eq, Ord, Enum, Hashable, Data, Typeable, Num)

$(deriveSafeCopy 0 'base ''ReviewId)

newtype UserId = UserId
  { getUserId :: Integer
  } deriving (Show, Eq, Ord, Enum, Hashable, Data, Typeable, Num)

$(deriveSafeCopy 0 'base ''UserId)

-- * Reviews


data Review = Review
  { reviewId   :: ReviewId -- For JSON?
  , postDate   :: UTCTime
  , author     :: UserId
  , reviewText :: LT.Text
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Review)


-- * Users

data User = User
  { userId            :: UserId -- For JSON? May be unneccesary
  , userVotesPackages :: StorableStrictHashMap PackageName Vote
  , userVotesReviews  :: StorableStrictHashMap PackageName
                           (StorableStrictHashMap ReviewId Vote)
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
  empty = ixSet
    [ ixFun $ \u -> [userId u]
    , ixFun $ \u -> HMS.keys . getSSHashMap . userVotesPackages $ u
    , ixFun $ \u -> HMS.keys . getSSHashMap
                <=< HMS.elems . getSSHashMap . userVotesReviews $ u
    ]
