{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  #-}

module Schema.Types where

{- Globally recognized storable data -}

import Data.Aeson
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

instance (FromJSON k, FromJSON a, Eq k, Hashable k
         ) => FromJSON (StorableStrictHashMap k a) where
  parseJSON x = (StorableStrictHashMap . HMS.fromList) <$> parseJSON x

instance (ToJSON k, ToJSON a) => ToJSON (StorableStrictHashMap k a) where
  toJSON (StorableStrictHashMap xs) = toJSON (HMS.toList xs)

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

instance (FromJSON k, FromJSON a, Eq k, Hashable k
         ) => FromJSON (StorableLazyHashMap k a) where
  parseJSON x = (StorableLazyHashMap . HML.fromList) <$> parseJSON x

instance (ToJSON k, ToJSON a) => ToJSON (StorableLazyHashMap k a) where
  toJSON (StorableLazyHashMap xs) = toJSON (HML.toList xs)


newtype StorableHashSet a = StorableHashSet
  { getStorableHashSet :: HS.HashSet a
  } deriving (Show, Eq, Data, Typeable)

instance (Ord a, Hashable a) => Ord (StorableHashSet a) where
  compare (StorableHashSet xs) (StorableHashSet ys) =
    compare (HS.toList xs) (HS.toList ys)

instance (Eq a, Hashable a, SafeCopy a) => SafeCopy (StorableHashSet a) where
  putCopy (StorableHashSet xs) = contain . safePut . HS.toList $ xs
  getCopy = contain $ (StorableHashSet . HS.fromList) <$> safeGet

instance (Eq a, Hashable a, FromJSON a) => FromJSON (StorableHashSet a) where
  parseJSON xs = (StorableHashSet . HS.fromList) <$> parseJSON xs

instance ToJSON a => ToJSON (StorableHashSet a) where
  toJSON (StorableHashSet x) = toJSON $ HS.toList x
