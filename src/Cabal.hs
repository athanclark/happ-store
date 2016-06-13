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
import Data.Acid.Advanced (query', update')
import Control.Monad.Reader
import Control.Monad.ST

import Debug.Trace


-- | Given a package name and version, try and fetch the cabal description
fetchCabal :: Env -> PackageName -> Version -> IO (Maybe GenericPackageDescription)
fetchCabal env (PackageName package) (Version version) = do
  let manager = envManager env
      v = intercalate "." $ show <$> version
  request  <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ T.unpack package ++ "-" ++ v ++ "/"
                      ++ T.unpack package ++ ".cabal"
  response <- httpLbs request manager
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
  parseJSON (String s) = pure . PackageListing . PackageName $ s
  parseJSON v = fail $ "Not an object: " ++ show v

fetchAllPackages :: Env -> IO [PackageName]
fetchAllPackages env = do
  let manager = envManager env
  request  <- parseUrl "https://hackage.haskell.org/packages/"
  let req = request { requestHeaders = [("Accept", "application/json")] }
  response <- httpLbs req manager
  pure $ case eitherDecode . responseBody $ response of
           Left e -> traceShow e []
           Right xs -> getPackageListing <$> xs



-- TODO: Collect database of download stats routinely
--       download stats may need to be scraped from HTML :\


fetchBatch :: Env -> IO BatchFetched
fetchBatch env = do
  depr <- fetchDeprecated env
  dist <- fetchDistros env
  docs <- fetchDocs env
  return $ BatchFetched depr dist docs

updateSpecific :: Env -> PackageName -> SpecificFetched -> IO SpecificFetched
updateSpecific env p xs = do
  putStrLn $ "Fetching data for package " ++ show p
  mvs <- fetchVersions env p
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
        mut <- fetchUploadTime env p newVersion
        pure $
          case mut of
            Nothing -> xs -- something slipped with hackage
            Just ut -> xs { fetchedVersions   = HMS.insert p vs (fetchedVersions xs)
                          , fetchedUploadTime = HMS.insert p ut (fetchedUploadTime xs)
                          }


-- | Don't fetch packages that are already known of, even if there may be
--   new versions out
updateFetchedShallow :: Env -> IO ()
updateFetchedShallow env = do
  let fetchedRef = envFetched env
  fetched' <- stToIO $ readSTRef fetchedRef
  batch'   <- fetchBatch env
  let fetched = fetched' { batch = batch' }
      db = envDatabase env
  (StorableHashSet knownPackages) <- query' db CurrentKnownPackages
  allPackages <- HS.fromList <$> fetchAllPackages env
  let diffPackages = allPackages `HS.difference` knownPackages
  when (diffPackages /= HS.empty) $
    let go p = do
          sp <- specific <$> stToIO (readSTRef fetchedRef)
          sp' <- updateSpecific env p sp
          let newFetched = fetched { specific = sp' }
          stToIO $ writeSTRef fetchedRef newFetched
          mPackage <- makePackage env p
          case mPackage of
            Nothing -> pure ()
            Just package -> update' (envDatabase env) (InsertPackage package)
    in mapM_ go diffPackages


-- | Disregard batch jobs, and spam hackage with version requests :\
updateFetchedDeep :: Env -> IO ()
updateFetchedDeep env = do
  let fetchedRef = envFetched env
  fetched <- stToIO $ readSTRef fetchedRef
  allPackages <- HS.fromList <$> fetchAllPackages env
  newSpecificFetched <- foldrM (updateSpecific env) (specific fetched) allPackages
  let newFetched = fetched { specific = newSpecificFetched }
  stToIO $ writeSTRef fetchedRef newFetched


-- | This assumes the fetch cache is already built
makePackage :: Env -> PackageName -> IO (Maybe Package)
makePackage env package = do
  let fetchedRef = envFetched env
  fetched <- stToIO $ readSTRef fetchedRef
      -- if specific defs fail, the package can't be built
  let specificData = specific fetched
      mUpload = HMS.lookup package $ fetchedUploadTime specificData
      mVersions = HMS.lookup package $ fetchedVersions specificData
  case (do vs <- mVersions
           v <- latestVersion vs
           pure (vs, v)
       ) of
    Nothing -> do putStrLn "blast!"
                  pure Nothing
    Just (versions, version) -> do
      let batchData = batch fetched
          mDepr = HMS.lookup package $ fetchedDeprecated batchData
          dists = StorableStrictHashMap . fromMaybe HMS.empty $
                    HMS.lookup package $ fetchedDistros batchData
          mDocs = HMS.lookup package $ fetchedDocs batchData

      mDescription <- fetchCabal env package version
      pure $ do
        upload <- mUpload
        p <- C.packageDescription <$> mDescription
        Just Package
               { name          = PackageName . T.pack . unPackageName
                                             . pkgName . C.package $ p
               , author        = T.pack $ C.author p
               , versions      = versions
               , maintainer    = T.pack $ C.maintainer p
               , license       = C.license p
               , copyright     = T.pack $ C.copyright p
               , synopsis      = T.pack $ C.synopsis p
               , categories    = StorableHashSet $
                                   HS.fromList . fmap T.strip . T.splitOn ","
                                               . T.pack . C.category $ p
               , stability     = T.pack $ C.stability p
               , homepage      = let h = C.homepage p
                                 in if h == "" then Nothing else Just $ T.pack h
               , sourceRepos   = C.sourceRepos p
               , isDeprecated  = mDepr
               , docs          = mDocs
               , distributions = dists
               , uploadedAt    = upload
               }
