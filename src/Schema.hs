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
import Data.SafeCopy                 ( SafeCopy (..), contain, safePut, safeGet, base
                                     , deriveSafeCopy)
import Data.IxSet                    (Indexable (empty), IxSet, ixFun, ixSet)
import qualified Data.IxSet          as IxSet
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.HashSet        as HS
import Data.Hashable                 (Hashable)

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

-- newtype GoogleUserId = GoogleUserId
--   { getGoogleUserId :: T.Text
--   } deriving (Show, Eq, Ord, Generic, Data, Typeable)
-- 
-- $(deriveSafeCopy 0 'base ''GoogleUserId)
-- 
-- instance Hashable GoogleUserId
-- 
-- 
-- data LoginMethod
--   = LoginGoogle GoogleUserId
--   deriving (Show, Eq, Ord, Generic, Data, Typeable)
-- 
-- $(deriveSafeCopy 0 'base ''LoginMethod)
-- 
-- instance Hashable LoginMethod


-- * User

-- * Cabal Packages

