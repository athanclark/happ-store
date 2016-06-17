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
import qualified Data.Set            as Set
import qualified Data.Map.Strict     as MapS
import qualified Data.IntMap.Lazy    as IntMap
import qualified Data.IntMap.Strict  as IntMapS
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State



type PackageId = Int

data Packages = Packages
  { packageId          :: {-# UNPACK #-} !PackageId
  , allPackages        :: {-# UNPACK #-} !(IntMap.IntMap Package)
  , latestVersions     :: {-# UNPACK #-} !(IntMapS.IntMap Version)
  , packagesByName     :: {-# UNPACK #-} !(MapS.Map PackageName PackageId)
  , packagesByAuthor   :: {-# UNPACK #-} !(MapS.Map Author   (Set.Set PackageId))
  , packagesByCategory :: {-# UNPACK #-} !(MapS.Map Category (Set.Set PackageId))
  , packagesByDistro   :: {-# UNPACK #-} !(MapS.Map Distro   (MapS.Map PackageId (Version, T.Text)))
  } deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Packages)

initPackages :: Packages
initPackages = Packages
  { packageId = 0
  , allPackages = IntMap.empty
  , latestVersions = IntMapS.empty
  , packagesByName = MapS.empty
  , packagesByAuthor = MapS.empty
  , packagesByCategory = MapS.empty
  , packagesByDistro = MapS.empty
  }

addPackage :: Package -> Packages -> Packages
addPackage package packages =
  case MapS.lookup (name package) (packagesByName packages) of
    Nothing ->
      let pId = packageId packages
      in  Packages
            { packageId          = packageId packages + 1
            , allPackages        = IntMap.insert pId package
                                 $ allPackages packages
            , latestVersions     = IntMapS.insert pId
                                     (Set.findMax . allVersions $ versions package)
                                 $ latestVersions packages
            , packagesByName     = MapS.insert (name package) pId
                                 $ packagesByName packages
            , packagesByAuthor   = MapS.insertWith
                                     Set.union
                                     (Cabal.Types.author package)
                                     (Set.singleton pId)
                                 $ packagesByAuthor packages
            , packagesByCategory = foldr (\k -> MapS.insertWith Set.union k
                                                  $ Set.singleton pId)
                                     (packagesByCategory packages)
                                 $ categories package
            , packagesByDistro   = MapS.unionWith
                                     MapS.union
                                     (packagesByDistro packages)
                                     (MapS.singleton pId <$> distributions package)
            }
    Just pId ->
      packages
        { allPackages = IntMap.insert pId package $ allPackages packages
        , latestVersions     = IntMapS.insert pId
                                 (Set.findMax . allVersions $ versions package)
                             $ latestVersions packages
        , packagesByCategory = foldr (\k -> MapS.insertWith Set.union k
                                              $ Set.singleton pId)
                                 (packagesByCategory packages)
                             $ categories package
        , packagesByDistro = MapS.unionWith
                               MapS.union
                               (packagesByDistro packages)
                               (MapS.singleton pId <$> distributions package)
        }


data Database = Database
  { nextReviewId  :: ReviewId -- for fresh ids
  , nextUserId    :: UserId   -- samesies
  , packages      :: Packages
  , users         :: IxSet User
  , reviews       :: StorableLazyHashMap ReviewId Review
  } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Database)

currentKnownPackages :: Query Database (Set.Set PackageName)
currentKnownPackages =
  MapS.keysSet <$> currentKnownPackageVersions

-- Should be unique in PackageName
currentKnownPackageVersions :: Query Database (MapS.Map PackageName Version)
currentKnownPackageVersions = do
  ps <- packages <$> ask
  let names = packagesByName ps
      vs = latestVersions ps
  pure $ fromJust . flip IntMapS.lookup vs <$> names

lookupPackage :: PackageName -> Query Database (Maybe Package)
lookupPackage packageName = do
  db <- ask
  let ps = packages db
  pure $ flip IntMap.lookup (allPackages ps)
     =<< MapS.lookup packageName (packagesByName ps)

-- | Overwrites with the latest version
insertPackage :: Package -> Update Database ()
insertPackage package =
  modify $ \db ->
    db { packages = addPackage package $ packages db
       }


--subjectiveVoteOfPackage :: UserId -> PackageName -> Int -> Query Database Vote
--subjectiveVoteOfPackage uid package depth = do
--  db <- ask
--  if depth == 0
--  then pure . fromMaybe mempty $ do
--         user <- getOne $ users db @= uid
--         HMS.lookup package . getSSHashMap $ userVotesPackages user
--  else
--    case getOne $ users db @= uid of
--      Nothing -> pure mempty
--      Just user -> do
--        let trustedReviewIds :: HMS.HashMap ReviewId Vote
--            trustedReviewIds = fromMaybe HMS.empty
--                             $ getSSHashMap <$> HMS.lookup package
--                               (getSSHashMap $ userVotesReviews user)
--            -- by how much you trust their opinion
--            trustedUsers :: HMS.HashMap UserId Vote
--            trustedUsers =
--              let reviews' :: [(Review, Vote)]
--                  reviews' = (\(r,v) -> ( fromJust . HML.lookup r . getSLHashMap
--                                                   $ reviews db
--                                        , v))
--                         <$> HMS.toList trustedReviewIds
--              in  foldr (\(k,v) -> HMS.insertWith (<>) (Server.author k) v)
--                    HMS.empty reviews'
--        userOpinions <- traverse (\u ->
--                          (u,) <$> subjectiveVoteOfPackage u package (depth - 1)
--                        )
--                      $ HMS.keys trustedUsers :: Query Database [(UserId, Vote)]
--        let adjustedUserOpinions =
--              HMS.unionWith (<>) trustedUsers $ HMS.fromList userOpinions
--        pure $
--          let everyoneElsesOpinion = fold adjustedUserOpinions
--              theirOpinion = fromMaybe mempty . HMS.lookup package
--                           . getSSHashMap $ userVotesPackages user
--          in  everyoneElsesOpinion <> theirOpinion

$(makeAcidic ''Database
    [ 'currentKnownPackages
    , 'currentKnownPackageVersions
    , 'lookupPackage
    , 'insertPackage
    ])

initDB :: Database
initDB = Database
  { nextReviewId  = 0
  , nextUserId    = 0
  , packages      = initPackages
  , users         = IxSet.empty
  , reviews       = StorableLazyHashMap HML.empty
  }
