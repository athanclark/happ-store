{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , GeneralizedNewtypeDeriving
  #-}

module Schema where

import Imports

import qualified Data.Text as T

import Data.SafeCopy (SafeCopy (..), contain, safePut, safeGet, base, deriveSafeCopy)
import Data.IxSet
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM

import Cooperate.Types (BeliefSet)

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
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''GoogleUserId)


data LoginMethod
  = LoginGoogle GoogleUserId
  deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''LoginMethod)



-- * Bookkeeping

newtype UserId = UserId
  { getUserId :: Int
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''UserId)

newtype SessionId = SessionId
  { getSessionId :: T.Text
  } deriving (Show, Eq, Ord, Data, Typeable, Hashable)

$(deriveSafeCopy 0 'base ''SessionId)


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


data User = User
  { userId         :: UserId
  , userSession    :: Maybe SessionId
  , userName       :: T.Text
  , userLogins     :: [LoginMethod]
  , userReferences :: References
  , userBeliefs    :: BeliefSet
  } deriving (Show, Eq, Data, Typeable)

$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
  empty = ixSet
    [ ixFun $ \u -> [userId u]
    , ixFun $ \u -> [userSession u]
    , ixFun $ \u -> [userName u]
    , ixFun $ \u -> userLogins u
    , ixFun $ \u -> HM.keys . getReferences $ userReferences u
    ]

