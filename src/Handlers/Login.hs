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
import Crypto.Hash
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.UTF8   as BSU8
import Data.ByteArray (convert)
import Data.UUID as UUID
import Data.UUID as UUID
import Data.UUID.V4 as UUID



data LoginCredentials = LoginPlain
      { username :: String
      , password :: String
      }
  deriving (Show, Eq)

instance FromJSON LoginCredentials where
  parseJSON (Object o) = do
    u <- o .: "username"
    p <- o .: "password"
    return $ LoginPlain u p
  parseJSON _ = empty

instance ToJSON LoginCredentials where
  toJSON ls =
    object
      [ "username" .= username ls
      , "password" .= password ls
      ]


checkLoginRequest :: ( MonadApp m
                     ) => SessionRequest LoginCredentials
                       -> m (SessionRequest T.Text)
checkLoginRequest s = do
  let data'  = BSL.toStrict . A.encode $ sessionData s
      nonce' = BSU8.fromString . UUID.toString $ sessionNonce s
      expectedHash :: Digest SHA512
      expectedHash = hash $ nonce' <> data'
  when (convert expectedHash /= sessionHash s) $ throwM InvalidLoginHash
  -- TODO: lookup user, etc.
  sessionCache <- envSession <$> ask
  newNonce <- liftIO UUID.nextRandom
  let payload   = "login success!" :: T.Text
      payload'  = BSL.toStrict $ A.encode payload
      newNonce' = BSU8.fromString $ UUID.toString newNonce
      newHash  :: Digest SHA512
      newHash   = hash $ newNonce' <> payload' <> BS64.encode (convert expectedHash)
  liftIO $
    TM.insert newNonce (convert newHash) sessionCache
  return $ SessionRequest newNonce payload $ convert newHash


loginHandle :: MonadApp m
            => MiddlewareT m
loginHandle app req respond =
  let handle = action $
        post $ do
          ml <- liftIO $ A.decode <$> strictRequestBody req
          l  <- case ml of
                  Nothing -> throwM BadLoginFormat
                  Just x  -> pure x
          l' <- checkLoginRequest l
          json l'
  in  (handle `catchMiddlewareT` errorCatcher) app req respond

errorCatcher :: MonadApp m
             => LoginException -> MiddlewareT m
errorCatcher e app req respond =
  case e of
    InvalidLoginHash ->
      respond $ textOnly "Invalid Login Hash!" status403 []
    BadLoginFormat ->
      respond $ textOnly "Malformed Data" status400 []

loginFacebookHandle :: MonadApp m
                    => MiddlewareT m
loginFacebookHandle = action homeHandle
