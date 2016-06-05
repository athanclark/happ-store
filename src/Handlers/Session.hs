{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Session where

import Handlers.Chunks
import Handlers.App

import Session

import Imports
import qualified Data.Text as T
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
  in  handle app req respond
