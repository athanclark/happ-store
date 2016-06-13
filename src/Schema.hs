{-# LANGUAGE
    TemplateHaskell
  , DeriveDataTypeable
  #-}

module Schema where

import Schema.Types
import Cabal.Types
import Server.Types

import Data.Acid
import Data.IxSet as IxSet
import Data.SafeCopy hiding (Version)
import Data.Data
import qualified Data.HashMap.Lazy as HML


data Database = Database
  { nextReviewId  :: ReviewId -- for fresh ids
  , nextUserId    :: UserId   -- samesies
  , packages      :: IxSet Package
  , users         :: IxSet User
  , reviews       :: StorableLazyHashMap ReviewId Review
  , knownPackages :: [PackageName]
  } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Database)

$(makeAcidic ''Database [])

initDB :: Database
initDB = Database
  { nextReviewId  = 0
  , nextUserId    = 0
  , packages      = IxSet.empty
  , users         = IxSet.empty
  , reviews       = StorableLazyHashMap HML.empty
  , knownPackages = []
  }
