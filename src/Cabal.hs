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
import Schema
import Schema.Types
import Imports hiding (requestHeaders, responseStatus, author)

import           Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as C
import           Distribution.PackageDescription.Parse
import           Distribution.Package hiding (Package, PackageName)

import Network.HTTP.Client

import Data.Aeson
import Data.Foldable (foldrM)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as LBSU8
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HMS
import           Data.Set ((\\))
import qualified Data.Set            as Set
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.STRef
import Data.Acid.Advanced (query')
import Control.Monad.Reader
import Control.Monad.ST


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


newtype PackageListing = PackageListing
  { getPackageListing :: PackageName
  } deriving (Show, Eq)

instance FromJSON PackageListing where
  parseJSON (Object o) = o .: "packageName"
  parseJSON _ = fail "Not an object"

fetchAllPackages :: MonadApp m => m [PackageName]
fetchAllPackages = do
  manager  <- envManager <$> ask
  request  <- parseUrl "https://hackage.haskell.org/packages/"
  let req = request { requestHeaders = [("Accept", "application/json")] }
  response <- liftIO $ httpLbs req manager
  pure $ case decode . responseBody $ response of
           Nothing -> []
           Just xs -> getPackageListing <$> xs


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


fetchBatch :: MonadApp m => m BatchFetched
fetchBatch = do
  depr <- fetchDeprecated
  dist <- fetchDistros
  docs <- fetchDocs
  return $ BatchFetched depr dist docs

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


-- | Don't fetch packages that are already known of, even if there may be
--   new versions out
updateFetchedShallow :: MonadApp m => m ()
updateFetchedShallow = do
  fetchedRef <- envFetched <$> ask
  fetched' <- liftIO . stToIO . readSTRef $ fetchedRef
  batch' <- fetchBatch
  let fetched = fetched' { batch = batch' }
  db <- envDatabase <$> ask
  (StorableHashSet knownPackages) <- query' db CurrentKnwonPackages
  allPackages <- HS.fromList <$> fetchAllPackages
  let diffPackages = allPackages `HS.difference` knownPackages
  newSpecificFetched <-
    if diffPackages /= HS.empty
    then foldrM updateSpecific (specific fetched) diffPackages
    else pure $ specific fetched
  let newFetched = fetched { specific = newSpecificFetched }
  liftIO . stToIO $ writeSTRef fetchedRef newFetched
