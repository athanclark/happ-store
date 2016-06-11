{-# LANGUAGE
    FlexibleContexts
  #-}

module Cabal where

import Imports hiding (responseStatus)

import           Distribution.PackageDescription (GenericPackageDescription, SourceRepo)
import qualified Distribution.PackageDescription as C
import           Distribution.PackageDescription.Parse
import           Distribution.License
import           Distribution.Package hiding (Package)
import           Distribution.Version

import Network.HTTP.Client
import Network.HTTP.Types.Status

import qualified Data.ByteString.Lazy.UTF8 as LBSU8
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
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
  { name        :: String
  , version     :: [Int]
  , author      :: String
  , maintainer  :: String
  , license     :: License
  , copyright   :: String
  , synopsis    :: String
  , categories  :: [String]
  , stability   :: String
  , homepage    :: Maybe String
  , sourceRepos :: [SourceRepo]
  -- , isPreferred :: Bool
  -- , isDeprecated :: Bool
  -- , rating :: ? Votes?
  -- , tags :: ... user suggested also, maybe just a sum type or something
  -- , docs :: ... Bool?
  -- , downloads :: Int
  -- , reviews :: [ReviewId] or something horrid
           -- egad, rating reviews too?
  -- , uploadedAt :: UTCTime
  -- , distributions :: NixOS | Stackage | LTSHaskell | ...
  }

makePackage :: GenericPackageDescription -> Package
makePackage xs =
  let p = C.packageDescription xs
  in  Package { name        = unPackageName . pkgName    . C.package $ p
              , version     = versionBranch . pkgVersion . C.package $ p
              , author      = C.author p
              , maintainer  = C.maintainer p
              , license     = C.license p
              , copyright   = C.copyright p
              , synopsis    = C.synopsis p
              , categories  = strip <$> splitOn "," (C.category p)
              , stability   = C.stability p
              , homepage    = let h = C.homepage p
                              in if h == "" then Nothing else Just h
              , sourceRepos = C.sourceRepos p
              }



-- TODO: Collect database of deprecated packages routinely, like every few days
-- TODO: Collect database of preferred versions routinely, parse them and build a
--       predicate to see if a version is preferred or not
-- TODO: Collect database of download stats routinely
-- TODO: Collect database of docs stats
-- TODO: use /packages/ to know the full set of packages available
-- TODO: use /package/:package-version/upload-time to find the upload time
-- TODO: download stats may need to be scraped from HTML :\
-- TODO: use /distro/:distro/packages to reverse-key assign each package its latest
--       designated distro available
