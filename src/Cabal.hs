{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module Cabal where

import Cabal.Types
import Cabal.Deprecated
import Cabal.Docs
import Cabal.Distros
import Cabal.Uploaded
import Cabal.Versions
import Schema.Types
import Imports hiding (responseStatus, author)

import           Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as C
import           Distribution.PackageDescription.Parse
import           Distribution.Package hiding (Package, PackageName)

import Network.HTTP.Client

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as LBSU8
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HMS
import           Data.Set ((\\))
import qualified Data.Set            as Set
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time
import Control.Monad.Reader


-- | Given a package name and version, try and fetch the cabal description
fetchCabal :: MonadApp m => PackageName -> Version -> m (Maybe GenericPackageDescription)
fetchCabal (PackageName package) (Version version) = do
  manager  <- envManager <$> ask
  let v = intercalate "." $ show <$> version
  request  <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ T.unpack package ++ "-" ++ v ++ "/"
                      ++ T.unpack package ++ ".cabal"
  response <- liftIO $ httpLbs request manager
  if responseStatus response /= status200
  then pure Nothing
  else case parsePackageDescription . LBSU8.toString . responseBody $ response of
         ParseFailed e -> pure Nothing
         ParseOk _ x   -> pure (Just x)


-- TODO: Turn into a maker - needs globally scraped data, plus package-specific ones
addCabalDesc :: GenericPackageDescription -> Package -> Package
addCabalDesc xs package =
  let p = C.packageDescription xs
  in  package { name        = PackageName . T.pack . unPackageName
                                          . pkgName . C.package $ p
              , author      = T.pack $ C.author p
              , maintainer  = T.pack $ C.maintainer p
              , license     = C.license p
              , copyright   = T.pack $ C.copyright p
              , synopsis    = T.pack $ C.synopsis p
              , categories  = StorableHashSet $
                  let xs = HS.fromList . fmap T.strip . T.splitOn ","
                                       . T.pack . C.category $ p
                  in  getStorableHashSet (categories package) `HS.union` xs
              , stability   = T.pack $ C.stability p
              , homepage    = let h = C.homepage p
                              in if h == "" then Nothing else Just $ T.pack h
              , sourceRepos = C.sourceRepos p
              }


-- TODO: Collect database of download stats routinely
--       download stats may need to be scraped from HTML :\
-- TODO: use /packages/ to know the full set of packages available


data BatchFetched = BatchFetched
  { fetchedDeprecated :: HMS.HashMap PackageName [PackageName]
  , fetchedDistros    :: HMS.HashMap PackageName (HMS.HashMap Distro (Version, T.Text))
  , fetchedDocs       :: HMS.HashMap PackageName Version
  } deriving (Show, Eq)

data SpecificFetched = SpecificFetched
  { fetchedUploadTime :: HMS.HashMap PackageName UTCTime -- latest only
  , fetchedVersions   :: HMS.HashMap PackageName Versions
  } deriving (Show, Eq)

updateSpecific :: MonadApp m => PackageName -> SpecificFetched -> m SpecificFetched
updateSpecific p xs = do
  mvs <- fetchVersions p
  case mvs of
    Nothing -> pure xs -- package doesn't exist
    Just vs ->
      let newVersions = allVersions vs
          oldVersions = fromMaybe Set.empty
                      $ allVersions <$> HMS.lookup p (fetchedVersions xs)
          moreVersions = newVersions \\ oldVersions
      in if moreVersions == Set.empty
      then pure xs
      else let newVersion = Set.findMax newVersions
      in do
        mut <- fetchUploadTime p newVersion
        pure $
          case mut of
            Nothing -> xs -- something slipped with hackage
            Just ut -> xs { fetchedVersions   = HMS.insert p vs (fetchedVersions xs)
                          , fetchedUploadTime = HMS.insert p ut (fetchedUploadTime xs)
                          }
