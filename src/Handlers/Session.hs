{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Session where

import Handlers.Chunks
import Handlers.App

import Imports
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Crypto.Hash
import Data.ByteString.Base64
import Data.UUID.V4
import Data.UUID as UUID




sessionHandle :: MonadApp m
              => MiddlewareT m
sessionHandle = action $ do
  post uploader $
    json ("ayyy" :: T.Text)
  where
    uploader :: MonadApp m => Request -> m ()
    uploader r = do
      liftIO $ print =<< strictRequestBody r
      return ()
