{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Cabal.Versions where

import Cabal.Types
import Imports hiding (requestHeaders)

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Client
import Control.Monad.Catch
import Control.Monad.Reader




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

