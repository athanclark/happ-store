{-# LANGUAGE
    TemplateHaskell
  , DeriveDataTypeable
  #-}

module Schema where

import Schema.Types
import Cabal.Types
import Server.Types

import Data.Acid
import Data.IxSet
import Data.SafeCopy hiding (Version)
import Data.Data


data Database = Database
  { reviewId      :: ReviewId -- for fresh ids
  , userId        :: UserId   -- samesies
  , packages      :: IxSet Package
  , users         :: IxSet User
  , reviews       :: StorableLazyHashMap ReviewId Review
  , knownPackages :: [PackageName]
  } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Database)
