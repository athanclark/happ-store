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


data DeprecatedPackage = DeprecatedPackage
  { packageName  :: PackageName
  , replacements :: [PackageName]
  } deriving (Show, Eq)

instance FromJSON DeprecatedPackage where
  parseJSON (Object o) =
    DeprecatedPackage <$> o .: "deprecated-package"
                      <*> o .: "in-favour-of"
  parseJSON _ = fail "Not an object"


data DeprecatedError
  = DeprecatedNoParse LBS.ByteString
  deriving (Show, Eq, Generic)

instance Exception DeprecatedError

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
