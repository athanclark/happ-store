{-# LANGUAGE
    TemplateHaskell
  , DeriveDataTypeable
  , TypeFamilies
  , TupleSections
  #-}

module Schema where

import Schema.Types
import Cabal.Types
import Server.Types as Server

import Data.Acid
import Data.IxSet as IxSet
import Data.SafeCopy hiding (Version)
import Data.Data
import Data.Monoid
import Data.Maybe (fromMaybe, fromJust)
import Data.Foldable (fold)
import qualified Data.HashMap.Lazy   as HML
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
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


subjectiveRatingOfPackage :: UserId -> PackageName -> Int -> Query Database Vote
subjectiveRatingOfPackage uid package depth = do
  db <- ask
  if depth == 0
  then pure . fromMaybe mempty $ do
         user <- getOne $ users db @= uid
         HMS.lookup package . getSSHashMap $ userVotesPackages user
  else
    case getOne $ users db @= uid of
      Nothing -> pure mempty
      Just user -> do
        let trustedReviewIds :: HMS.HashMap ReviewId Vote
            trustedReviewIds = fromMaybe HMS.empty
                             $ getSSHashMap <$> HMS.lookup package
                               (getSSHashMap $ userVotesReviews user)
            -- by how much you trust their opinion
            trustedUsers :: HMS.HashMap UserId Vote
            trustedUsers =
              let reviews' :: [(Review, Vote)]
                  reviews' = (\(r,v) -> ( fromJust . HML.lookup r . getSLHashMap
                                                   $ reviews db
                                        , v))
                         <$> HMS.toList trustedReviewIds
              in  foldr (\(k,v) -> HMS.insertWith (<>) (Server.author k) v)
                    HMS.empty reviews'
        userOpinions <- traverse (\u ->
                          (u,) <$> subjectiveRatingOfPackage u package (depth - 1)
                        )
                      $ HMS.keys trustedUsers :: Query Database [(UserId, Vote)]
        let adjustedUserOpinions =
              HMS.unionWith (<>) trustedUsers $ HMS.fromList userOpinions
        pure $
          let everyoneElsesOpinion = fold adjustedUserOpinions
              theirOpinion = fromMaybe mempty . HMS.lookup package
                           . getSSHashMap $ userVotesPackages user
          in  everyoneElsesOpinion <> theirOpinion

$(makeAcidic ''Database
    [ 'currentKnownPackages
    , 'addKnownPackage
    , 'lookupPackage
    , 'insertPackage
    , 'subjectiveRatingOfPackage
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
