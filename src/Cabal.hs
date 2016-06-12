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
import Imports hiding (responseStatus)

import           Distribution.PackageDescription (GenericPackageDescription, SourceRepo)
import qualified Distribution.PackageDescription as C
import           Distribution.PackageDescription.Parse
import           Distribution.License
import           Distribution.Package hiding (Package, PackageName, Version)
import           Distribution.Version hiding (Version)

import Network.HTTP.Client
import Network.HTTP.Types.Status

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as LBSU8
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Data.Tree.Hash         as HT
import Data.Tree.Set          as STr
import Data.Time
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set     as Set
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)


fetchCabal :: MonadApp m => String -> [Int] -> m (Maybe GenericPackageDescription)
fetchCabal packageName version = do
  manager  <- envManager <$> ask
  let v = intercalate "." $ show <$> version
  request  <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ packageName ++ "-" ++ v ++ "/" ++ packageName ++ ".cabal"
  response <- liftIO $ httpLbs request manager
  if responseStatus response /= status200
  then pure Nothing
  else case parsePackageDescription . LBSU8.toString . responseBody $ response of
         ParseFailed e -> pure Nothing
         ParseOk _ x   -> pure (Just x)

data Package = Package
  { name          :: PackageName
  , versions      :: Set.Set (STr.SetTree Int)
  , author        :: T.Text
  , maintainer    :: T.Text
  , license       :: License
  , copyright     :: T.Text
  , synopsis      :: T.Text
  , categories    :: HS.HashSet T.Text
  , stability     :: T.Text
  , homepage      :: Maybe T.Text
  , sourceRepos   :: [SourceRepo]
  , isDeprecated  :: Bool
  , docs          :: Maybe Version -- hackage only :\
  , distributions :: HM.HashMap Distro (Version, T.Text)
  , uploadedAt    :: UTCTime
  -- , rating :: ? Votes?
  -- , tags :: ... user suggested also, maybe just a sum type or something
  -- , downloads :: Int
  -- , reviews :: [ReviewId] or something horrid
           -- egad, rating reviews too?
  -- TODO: Make versions their own thing: we shouldn't have a different package
  --       concept for every version
  }

addCabalDesc :: GenericPackageDescription -> Package -> Package
addCabalDesc xs package =
  let p = C.packageDescription xs
  in  package { name        = T.pack . unPackageName . pkgName . C.package $ p
              , author      = T.pack $ C.author p
              , maintainer  = T.pack $ C.maintainer p
              , license     = C.license p
              , copyright   = T.pack $ C.copyright p
              , synopsis    = T.pack $ C.synopsis p
              , categories  =
                  let xs = HS.fromList . fmap T.strip . T.splitOn ","
                                       . T.pack . C.category $ p
                  in  categories package `HS.union` xs
              , stability   = T.pack $ C.stability p
              , homepage    = let h = C.homepage p
                              in if h == "" then Nothing else Just $ T.pack h
              , sourceRepos = C.sourceRepos p
              }


-- TODO: Collect database of download stats routinely
-- TODO: download stats may need to be scraped from HTML :\
-- TODO: use /packages/ to know the full set of packages available
-- TODO: use /package/:package/preferred to know the set of versions for each package
    -- populate data from most recent normal version
-- TODO: use /package/:package-version/upload-time to find the upload time
