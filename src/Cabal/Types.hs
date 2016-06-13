{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TemplateHaskell
  , DeriveDataTypeable
  , DeriveGeneric
  , StandaloneDeriving
  #-}

module Cabal.Types where

import Schema.Types

import Data.Aeson
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Hashable
import Data.Attoparsec.Text as A
import qualified Data.Set     as Set
import Data.Tree.Set          as STr
import qualified Data.Version as V
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Data
import Data.IxSet
import Data.SafeCopy hiding (Version)
import Control.Applicative as Appl
import Control.Monad.Catch

import GHC.Generics

import Distribution.License
import Distribution.PackageDescription ( GenericPackageDescription
                                       , SourceRepo (..)
                                       , RepoType
                                       , RepoKind
                                       )


-- * Version String

newtype Version = Version
  { getVersion :: [Int]
  } deriving (Eq, Ord, Hashable, Data, Typeable)

$(deriveSafeCopy 0 'base ''Version)

instance Show Version where
  show (Version vs) = intercalate "." (show <$> vs)

-- Partial, may throw exception
parseVersion :: T.Text -> Maybe Version
parseVersion t = Version <$> mapM (readMaybe . T.unpack) (T.splitOn "." t)

instance FromJSON Version where
  parseJSON (String s) =
    case parseVersion s of
      Nothing -> fail "Not in the right format"
      Just v  -> pure v
  parseJSON _ = fail "Not a string"

parseVersionA :: Parser Version
parseVersionA = do
  xs <- (A.many1 A.digit <?> "Int") `A.sepBy1` (A.char '.' <?> "Separator")
  case mapM readMaybe xs of
    Nothing -> Appl.empty
    Just ys -> pure $ Version ys


-- * Package Names

newtype PackageName = PackageName
  { getPackageName :: T.Text
  } deriving (Show, Eq, Ord, Hashable, Data, Typeable, FromJSON, ToJSON)

$(deriveSafeCopy 0 'base ''PackageName)


-- * Distros

newtype Distro = Distro
  { getDistro :: T.Text
  } deriving (Show, Eq, Ord, Hashable, Data, Typeable)

$(deriveSafeCopy 0 'base ''Distro)


-- * Deprecated Versions

data DeprecatedPackage = DeprecatedPackage
  { packageName  :: PackageName
  , replacements :: [PackageName]
  } deriving (Show, Eq)

instance FromJSON DeprecatedPackage where
  parseJSON (Object o) =
    DeprecatedPackage <$> o .: "deprecated-package"
                      <*> o .: "in-favour-of"
  parseJSON _ = fail "Not an object"


-- * Preferred Constraints

data Greater
  = GreaterThan
  | GreaterThanEq
  deriving (Show, Eq)

data EqVersion
  = Cons Int EqVersion
  | Wildcard
  | Nil
  deriving (Show, Eq)

data Constraint
  = EqualTo EqVersion
  | Between { lowerB  :: Greater
            , lowerBV :: [Int]
            , upperBV :: [Int]
            }
  | Above   { lowerA  :: Greater
            , lowerAV :: [Int]
            }
  | Below [Int]
  deriving (Show, Eq)

-- a list of ||
type Preferred = [Constraint]


-- * Version Fetching

data Versions = Versions
  { normal      :: [Version]
  , deprecated  :: [Version]
  , unpreferred :: [Version]
  } deriving (Show, Eq)

instance FromJSON Versions where
  parseJSON (Object o) = do
    mn <- o .:? "normal-version"
    md <- o .:? "deprecated-version"
    mu <- o .:? "unpreferred-version"
    pure Versions
           { normal      = fromMaybe [] mn
           , deprecated  = fromMaybe [] md
           , unpreferred = fromMaybe [] mu
           }
  parseJSON _ = fail "Not an object"


-- * Package Data

newtype Author = Author
  { getAuthor :: T.Text
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Author)

newtype Maintainer = Maintainer
  { getMaintainer :: T.Text
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Maintainer)

newtype Category = Category
  { getCategory :: T.Text
  } deriving (Show, Eq, Ord, Hashable, Data, Typeable)

$(deriveSafeCopy 0 'base ''Category)

newtype Stability = Stability
  { getStability :: T.Text
  } deriving (Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Stability)


deriving instance Ord SourceRepo
deriving instance Ord License

data Package = Package
  { name          :: PackageName
  , versions      :: Set.Set (STr.SetTree Int)
  , author        :: T.Text
  , maintainer    :: T.Text
  , license       :: License
  , copyright     :: T.Text
  , synopsis      :: T.Text
  , categories    :: StorableHashSet T.Text
  , stability     :: T.Text
  , homepage      :: Maybe T.Text
  , sourceRepos   :: [SourceRepo]
  , isDeprecated  :: Bool
  , docs          :: Maybe Version -- hackage only :\
  , distributions :: StorableStrictHashMap Distro (Version, T.Text)
  , uploadedAt    :: UTCTime
  -- , rating :: ? Votes?
  -- , tags :: ... user suggested also, maybe just a sum type or something
  -- , downloads :: Int
  -- , reviews :: [ReviewId] or something horrid
           -- egad, rating reviews too?
  -- TODO: Make versions their own thing: we shouldn't have a different package
  --       concept for every version
  } deriving (Show, Eq, Ord, Data, Typeable)


instance (SafeCopy a, Ord a) => SafeCopy (SetTree a) where
  putCopy xs = putCopy (STr.toTree xs)
  getCopy = contain $ STr.fromTree <$> safeGet

$(deriveSafeCopy 0 'base ''V.Version)

$(deriveSafeCopy 0 'base ''License)

$(deriveSafeCopy 0 'base ''RepoKind)

$(deriveSafeCopy 0 'base ''RepoType)

$(deriveSafeCopy 0 'base ''SourceRepo)

$(deriveSafeCopy 0 'base ''Package)

instance Indexable Package where
  empty = ixSet
    [ ixFun $ \p -> [name p]
    , ixFun $ \p -> [author p]
    , ixFun $ \p -> [maintainer p]
    , ixFun $ \p -> [license p]
    , ixFun $ \p -> HS.toList . getStorableHashSet . categories $ p
    , ixFun $ \p -> HM.keys   . getSSHashMap    . distributions $ p
    ]




-- * Exceptions

data DeprecatedError
  = DeprecatedNoParse LBS.ByteString
  deriving (Show, Eq, Generic)

instance Exception DeprecatedError


data DocsError
  = DocsNoParse LBS.ByteString
  deriving (Show, Eq, Generic)

instance Exception DocsError


data PreferredError
  = ParseError String
  deriving (Eq, Show, Generic)

instance Exception PreferredError


data VersionsError
  = VersionsNoParse String LBS.ByteString
  deriving (Show, Eq, Generic)

instance Exception VersionsError
