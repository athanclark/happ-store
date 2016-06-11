{-# LANGUAGE
    FlexibleContexts
  #-}

module Cabal where

import Imports hiding (responseStatus)

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Network.HTTP.Client
import Network.HTTP.Types.Status

import qualified Data.ByteString.Lazy.UTF8 as LBSU8
import Data.List (intercalate)
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
