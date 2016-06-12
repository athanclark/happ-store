{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , FlexibleContexts
  #-}

module Cabal.Versions where

import Cabal.Types
import Imports hiding (requestHeaders)

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Data.HashMap.Strict as HM hiding (map, foldr, filter, null)
import Data.Maybe (fromMaybe)
import Control.Monad.Catch
import Control.Monad.Reader

import GHC.Generics



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


data VersionsError
  = VersionsNoParse String LBS.ByteString
  deriving (Show, Eq, Generic)

instance Exception VersionsError

fetchVersions :: MonadApp m => PackageName -> m Versions
fetchVersions package = do
  manager  <- envManager <$> ask
  request  <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ T.unpack package ++ "/preferred"
  let req = request
              { requestHeaders = [("Accept","application/json")] }
  response <- liftIO $ httpLbs req manager
  case eitherDecode (responseBody response) of
    Left  e  -> throwM . VersionsNoParse e . responseBody $ response
    Right xs -> pure xs

