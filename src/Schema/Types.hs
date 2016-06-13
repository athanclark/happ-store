{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , GeneralizedNewtypeDeriving
  #-}

module Schema.Types where

{-| Globally recognized storable data -}

import Data.SafeCopy
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy   as HML
import qualified Data.HashSet        as HS
import Data.Hashable                 (Hashable)
import Data.Data


newtype StorableStrictHashMap k a = StorableStrictHashMap
  { getSSHashMap :: HMS.HashMap k a
  } deriving (Show, Eq, Data, Typeable)

instance (Ord k, Ord a) => Ord (StorableStrictHashMap k a) where
  compare (StorableStrictHashMap xs) (StorableStrictHashMap ys) =
    compare (HMS.toList xs) (HMS.toList ys)

instance ( Eq k
         , Hashable k
         , SafeCopy k
         , SafeCopy a
         ) => SafeCopy (StorableStrictHashMap k a) where
  putCopy (StorableStrictHashMap xs) = contain . safePut . HMS.toList $ xs
  getCopy = contain $ (StorableStrictHashMap . HMS.fromList) <$> safeGet

newtype StorableLazyHashMap k a = StorableLazyHashMap
  { getSLHashMap :: HML.HashMap k a
  } deriving (Show, Eq, Data, Typeable)

instance (Ord k, Ord a) => Ord (StorableLazyHashMap k a) where
  compare (StorableLazyHashMap xs) (StorableLazyHashMap ys) =
    compare (HML.toList xs) (HML.toList ys)

instance ( Eq k
         , Hashable k
         , SafeCopy k
         , SafeCopy a
         ) => SafeCopy (StorableLazyHashMap k a) where
  putCopy (StorableLazyHashMap xs) = contain . safePut . HMS.toList $ xs
  getCopy = contain $ (StorableLazyHashMap . HMS.fromList) <$> safeGet


newtype StorableHashSet a = StorableHashSet
  { getStorableHashSet :: HS.HashSet a
  } deriving (Show, Eq, Data, Typeable)

instance (Ord a, Hashable a) => Ord (StorableHashSet a) where
  compare (StorableHashSet xs) (StorableHashSet ys) =
    compare (HS.toList xs) (HS.toList ys)

instance (Eq a, Hashable a, SafeCopy a) => SafeCopy (StorableHashSet a) where
  putCopy (StorableHashSet xs) = contain . safePut . HS.toList $ xs
  getCopy = contain $ (StorableHashSet . HS.fromList) <$> safeGet
