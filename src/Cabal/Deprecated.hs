{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Cabal.Deprecated where

import Cabal.Types
import Imports hiding (requestHeaders)

import Data.Aeson
import Network.HTTP.Client
import Control.Monad.Catch
import Control.Monad.Reader

import Data.HashMap.Strict as HM hiding (map)
import Control.Arrow
import Control.Concurrent



fetchDeprecated :: Env -> IO (HashMap PackageName [PackageName])
fetchDeprecated env =
  go `catch` (\e -> do print (e :: SomeException)
                       threadDelay 5000000
                       fetchDeprecated env
             )
  where
    go = do
      let manager = envManager env
      request <- parseUrl "https://hackage.haskell.org/packages/deprecated"
      let req = request { requestHeaders = [("Accept","application/json")] }
      response <- httpLbs req manager
      case decode (responseBody response) of
        Nothing -> throwM . DeprecatedNoParse . responseBody $ response
        Just xs -> pure . HM.fromList $ map (packageName &&& replacements) xs
