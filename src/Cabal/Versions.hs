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



fetchVersions :: MonadApp m => PackageName -> m Versions
fetchVersions (PackageName package) = do
  manager  <- envManager <$> ask
  request  <- parseUrl $ "https://hackage.haskell.org/package/"
                      ++ T.unpack package ++ "/preferred"
  let req = request
              { requestHeaders = [("Accept","application/json")] }
  response <- liftIO $ httpLbs req manager
  case eitherDecode (responseBody response) of
    Left  e  -> throwM . VersionsNoParse e . responseBody $ response
    Right xs -> pure xs

