{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , FlexibleContexts
  #-}

module Cabal.Deprecated where

import Cabal.Types
import Imports hiding (requestHeaders)

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Control.Monad.Catch
import Control.Monad.Reader

import Data.HashMap.Strict as HM hiding (map)
import Control.Arrow

import GHC.Generics


fetchDeprecated :: MonadApp m => m (HashMap PackageName [PackageName])
fetchDeprecated = do
  manager  <- envManager <$> ask
  request  <- parseUrl "https://hackage.haskell.org/packages/deprecated"
  let req = request
              { requestHeaders = [("Accept","application/json")] }
  response <- liftIO $ httpLbs req manager
  case decode (responseBody response) of
    Nothing -> throwM . DeprecatedNoParse . responseBody $ response
    Just xs -> pure . HM.fromList $ map (packageName &&& replacements) xs
