{-# LANGUAGE
    TemplateHaskell
  , DeriveDataTypeable
  , TypeFamilies
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
import qualified Data.HashSet      as HS
import Control.Monad.Reader


data Database = Database
  { nextReviewId  :: ReviewId -- for fresh ids
  , nextUserId    :: UserId   -- samesies
  , packages      :: IxSet Package
  , users         :: IxSet User
  , reviews       :: StorableLazyHashMap ReviewId Review
  , knownPackages :: StorableHashSet PackageName
  } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Database)

currentKnwonPackages :: Query Database (StorableHashSet PackageName)
currentKnwonPackages =
  knownPackages <$> ask


$(makeAcidic ''Database
    [ 'currentKnwonPackages
    ])

initDB :: Database
initDB = Database
  { nextReviewId  = 0
  , nextUserId    = 0
  , packages      = IxSet.empty
  , users         = IxSet.empty
  , reviews       = StorableLazyHashMap HML.empty
  , knownPackages = StorableHashSet     HS.empty
  }
