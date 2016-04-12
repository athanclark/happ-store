{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , GeneralizedNewtypeDeriving
  #-}

module Schema where

import Imports

import qualified Data.Text as T

import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Data.SafeCopy                 (SafeCopy (..), contain, safePut, safeGet, base, deriveSafeCopy)
import Data.IxSet                    (Indexable (empty), IxSet, ixFun, ixSet)
import qualified Data.IxSet          as IxSet
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.HashSet        as HS
import Data.Hashable                 (Hashable)

import Cooperate.Types   (BeliefSet (..), Belief, AgreementMeasure, BeliefMeasure (..))
import Cooperate.Measure (Measure)

import Data.Data
import GHC.Generics



-- TODO:
{-
- user system
  - openId logging shit, tokens, different authorities
    - no passwords, email registration or anything
  - Profile
    - some profile image, scraped from authorities? Gravatar?
    - links to their work
      - other people can add? :s how to not be framed?
- acid-state the library
  - IxSet user (BeliefSet user)?
-}

-- * Logins

newtype GoogleUserId = GoogleUserId
  { getGoogleUserId :: T.Text
  } deriving (Show, Eq, Ord, Generic, Data, Typeable)

$(deriveSafeCopy 0 'base ''GoogleUserId)

instance Hashable GoogleUserId


data LoginMethod
  = LoginGoogle GoogleUserId
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

$(deriveSafeCopy 0 'base ''LoginMethod)

instance Hashable LoginMethod


-- * User

-- | Public references for a particular person
data Reference
  = ReferenceTwitter  { getTwitterHandle  :: T.Text }
  | ReferenceBlog     { getBlogLink       :: T.Text }
  | ReferenceFacebook { getFacebookHandle :: T.Text }
  | ReferenceGoogle   { getGoogleHandle   :: T.Text }
  | ReferenceOther    { getOtherLink      :: T.Text }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

$(deriveSafeCopy 0 'base ''Reference)

instance Hashable Reference

-- | A mapping between plausibly affiliated original works, and the assumed
--   authors' endorsement
newtype References = References
  { getReferences :: HM.HashMap Reference Bool
  } deriving (Show, Eq, Data, Typeable)

instance SafeCopy References where
  putCopy (References xs) = contain . safePut $ HM.toList xs
  getCopy = contain $ (References . HM.fromList) <$> safeGet

newtype UserId = UserId
  { getUserId :: Int
  } deriving (Show, Eq, Ord, Data, Typeable, Hashable)

$(deriveSafeCopy 0 'base ''UserId)

data User = User
  { userName       :: T.Text
  , userReferences :: References
  , userBeliefs    :: BeliefSet UserId
  } deriving (Show, Eq, Data, Typeable)

$(deriveSafeCopy 0 'base ''User)


-- * Site-wide


newtype UserBase = UserBase
  { getUserBase :: HM.HashMap UserId User
  } deriving (Show, Eq, Data, Typeable)

instance SafeCopy UserBase where
  putCopy (UserBase xs) = contain . safePut $ HM.toList xs
  getCopy = contain $ (UserBase . HM.fromList) <$> safeGet



newtype LoginIndex = LoginIndex
  { getLoginIndex :: HM.HashMap LoginMethod UserId
  } deriving (Show, Eq, Data, Typeable)

instance SafeCopy LoginIndex where
  putCopy (LoginIndex xs) = contain . safePut $ HM.toList xs
  getCopy = contain $ (LoginIndex . HM.fromList) <$> safeGet



-- | All beliefs everyone's declared
newtype BeliefIndex = BeliefIndex
  { getBeliefIndex :: HM.HashMap (Belief UserId)
                                 (Map.Map AgreementMeasure
                                          (HS.HashSet UserId))
  } deriving (Show, Eq, Data, Typeable)

toListBI :: BeliefIndex -> [(Belief UserId, Map.Map AgreementMeasure [UserId])]
toListBI (BeliefIndex xs) = map (\(k, vs) -> (k, Map.map HS.toList vs)) $ HM.toList xs

fromListBI :: [(Belief UserId, Map.Map AgreementMeasure [UserId])] -> BeliefIndex
fromListBI xs = BeliefIndex . HM.fromList $ map (\(k, vs) -> (k, Map.map HS.fromList vs)) xs

instance Monoid BeliefIndex where
  mempty = BeliefIndex HM.empty
  mappend (BeliefIndex xs) (BeliefIndex ys) =
    BeliefIndex $ HM.unionWith (Map.unionWith HS.union) xs ys

instance SafeCopy BeliefIndex where
  putCopy xs = contain . safePut $ toListBI xs
  getCopy = contain $ fromListBI <$> safeGet


insertUser :: UserId
           -> [LoginMethod]
           -> User
           -> (UserBase, LoginIndex, BeliefIndex)
           -> (UserBase, LoginIndex, BeliefIndex)
insertUser k logins u (UserBase base, LoginIndex login, beliefs) =
  ( UserBase $ HM.insert k u base
  , LoginIndex $ foldr (\l acc -> HM.insert l k acc) login logins
  , let new = BeliefIndex $ (\m -> Map.singleton (beliefAgreement m) (HS.singleton k))
                         <$> getBeliefSet (userBeliefs u)
    in  new `mappend` beliefs
  )

deleteUser :: UserId
           -> (UserBase, LoginIndex, BeliefIndex)
           -> (UserBase, LoginIndex, BeliefIndex)
deleteUser k (UserBase base, LoginIndex login, BeliefIndex beliefs) =
  ( UserBase $ HM.delete k base
  , LoginIndex $ HM.filter (/= k) login
  , BeliefIndex $ HM.map (Map.map (HS.filter (/= k))) beliefs
  )

editUser :: UserId
         -> (User -> User)
         -> UserBase
         -> UserBase
editUser k f (UserBase base) = UserBase (HM.adjust f k base)


lookupUser :: UserId
           -> UserBase
           -> Maybe User
lookupUser k (UserBase base) = HM.lookup k base


addLogin :: UserId
         -> LoginMethod
         -> LoginIndex
         -> LoginIndex
addLogin k m (LoginIndex login) = LoginIndex (HM.insert m k login)

assertBelief :: UserId
             -> Belief UserId
             -> BeliefMeasure
             -> (UserBase, BeliefIndex)
             -> (UserBase, BeliefIndex)
assertBelief k b m (UserBase base, BeliefIndex beliefs) =
  ( UserBase $ HM.adjust (\u -> u { userBeliefs = BeliefSet $ HM.insert b m (getBeliefSet $ userBeliefs u)
                                  }) k base
  , BeliefIndex $ HM.alter go b beliefs
  )
  where
    go :: Maybe (Map.Map AgreementMeasure (HS.HashSet UserId))
       -> Maybe (Map.Map AgreementMeasure (HS.HashSet UserId))
    go mms = Just $ Map.update
                      (Just . HS.insert k)
                      (beliefAgreement m)
                      (fromMaybe Map.empty mms)

findAlike :: Belief UserId
          -> Int -- ^ Range
          -> AgreementMeasure
          -> BeliefIndex
          -> HS.HashSet UserId
findAlike b r m (BeliefIndex beliefs) =
  let ms = fromMaybe Map.empty (HM.lookup b beliefs)
      r' = fromIntegral $ r `div` 2
      (_, right) = Map.split (m - r') ms
      (mid, _) = Map.split (m + r') ms
  in  fold mid

findNotAlike :: Belief UserId
             -> Int -- ^ Range
             -> AgreementMeasure
             -> BeliefIndex
             -> HS.HashSet UserId
findNotAlike b r m (BeliefIndex beliefs) =
  let ms = fromMaybe Map.empty (HM.lookup b beliefs)
      r' = fromIntegral $ r `div` 2
      (left, _) = Map.split (m - r') ms
      (_, right) = Map.split (m + r') ms
  in  fold (Map.unionWith HS.union left right)
