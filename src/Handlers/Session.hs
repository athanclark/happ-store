{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Session
  ( sessionHandle
  ) where

import Handlers.Chunks
import Handlers.App

import Session

import Imports
import Data.Monoid
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Aeson as A hiding (json)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch

import Crypto.Hash
import Data.ByteString.Base64
import Data.UUID.V4
import Data.UUID as UUID




sessionHandle :: MonadApp m
              => MiddlewareT m
sessionHandle app req respond =
  let handle = action $
        post $ do
          ml <- liftIO $ A.decode <$> strictRequestBody req
          l  <- case ml of
                  Nothing -> throwM BadSessionFormat
                  Just x  -> pure (x :: SessionRequest T.Text)
          l' <- withSession l $ \_ ->
                  pure ("pong" :: T.Text)
          json l'
  in  (handle `catchMiddlewareT` errorCatcher) app req respond


errorCatcher :: MonadApp m
             => SessionException -> MiddlewareT m
errorCatcher e app req respond =
  case e of
    InvalidSessionHash ->
      respond $ textOnly "Invalid Session Hash!" status403 []
    BadSessionFormat ->
      respond $ textOnly "Malformed Data" status400 []
    NonexistentNonce n -> do
      liftIO . putStrLn $ "Nonce not found: " ++ show n
      respond $ textOnly ("Nonce not found: " <> LT.pack (show n)) status404 []
