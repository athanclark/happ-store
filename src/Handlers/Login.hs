{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Login
  ( loginHandle
  , loginFacebookHandle
  ) where

import Session

import Handlers.Chunks
import Handlers.App

import Imports

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Applicative
import Control.Monad
import Control.Concurrent.STM (atomically)
import Data.Monoid

import Data.Aeson as A hiding (json)
import Data.TimeMap as TM
import qualified Data.Text as T
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import Data.ByteArray (convert)



data LoginCredentials = LoginPlain
      { username :: String
      , password :: String
      }
  deriving (Show, Eq)

instance FromJSON LoginCredentials where
  parseJSON (Object o) =
    LoginPlain <$> (o .: "username") <*> (o .: "password")
  parseJSON _ = empty


checkLoginRequest :: ( MonadApp m
                     ) => SignedRequest
                       -> m Signature
checkLoginRequest x@(SignedRequest sId _) = do
  let f :: MonadApp m => LoginCredentials -> m T.Text
      f cs = pure "login success!" -- TODO: lookup user, etc.
  sessionCache <- envSession <$> ask
  liftIO $ TM.insert sId () sessionCache
  withSession f x


loginHandle :: MonadApp m
            => MiddlewareT m
loginHandle app req respond =
  let handle = action $
        post $ do
          mx <- liftIO $ A.decode <$> strictRequestBody req
          y  <- case mx of
                  Nothing -> throwM BadLoginFormat
                  Just x  -> checkLoginRequest x
          json y
  in  (handle `catchMiddlewareT` errorCatcher) app req respond

errorCatcher :: MonadApp m
             => LoginException -> MiddlewareT m
errorCatcher e app req respond =
  case e of
    BadLoginFormat ->
      respond $ textOnly "Malformed Data" status400 []

loginFacebookHandle :: MonadApp m
                    => MiddlewareT m
loginFacebookHandle = action homeHandle
