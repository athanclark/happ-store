{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Cabal.Versions where

import Cabal.Types
import Imports hiding (requestHeaders)

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Set  as Set
import Network.HTTP.Client
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Error
import Control.Concurrent


latestVersion :: Versions -> Maybe Version
latestVersion (Versions ns ds us) =
  let all' = us <> ds <> ns
  in  if all' == Set.empty
      then Nothing
      else Just $! Set.findMax all'

fetchVersions :: Env -> PackageName -> IO (Maybe Versions)
fetchVersions env (PackageName package) =
  go `catch` (\e -> do print (e :: SomeException)
                       threadDelay 5000000
                       fetchVersions env (PackageName package)
             )
  where
  go = do
    let manager = envManager env
    request <- parseUrl $ "https://hackage.haskell.org/package/"
                       ++ T.unpack package ++ "/preferred"
    let req = request
                { requestHeaders = [("Accept","application/json")] }
    response <- httpLbs req manager
    pure . hush . eitherDecode . responseBody $ response
