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
import Control.Monad.State


data Database = Database
  { nextReviewId  :: ReviewId -- for fresh ids
  , nextUserId    :: UserId   -- samesies
  , packages      :: IxSet Package
  , users         :: IxSet User
  , reviews       :: StorableLazyHashMap ReviewId Review
  , knownPackages :: StorableHashSet PackageName
  } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Database)

currentKnownPackages :: Query Database (StorableHashSet PackageName)
currentKnownPackages =
  knownPackages <$> ask

addKnownPackage :: PackageName -> Update Database ()
addKnownPackage p =
  modify (\db -> db { knownPackages = StorableHashSet . HS.insert p
                                    . getStorableHashSet . knownPackages $ db })

lookupPackage :: PackageName -> Query Database (Maybe Package)
lookupPackage package = do
  db <- ask
  pure . getOne $ packages db @= package

insertPackage :: Package -> Update Database ()
insertPackage package =
  modify (\db -> db { packages = IxSet.updateIx (name package) package $ packages db })

$(makeAcidic ''Database
    [ 'currentKnownPackages
    , 'addKnownPackage
    , 'lookupPackage
    , 'insertPackage
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
