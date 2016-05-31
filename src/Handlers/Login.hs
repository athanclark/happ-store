{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Login where

import Handlers.Chunks
import Handlers.App

import Imports
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)


loginHandle :: MonadApp m
            => MiddlewareT m
loginHandle = action $ do
  post uploader $ json ("ayyy" :: T.Text)
  where
    uploader :: MonadApp m => Request -> m ()
    uploader r = do
      liftIO $ print =<< strictRequestBody r
      return ()

loginFacebookHandle :: MonadApp m
                    => MiddlewareT m
loginFacebookHandle = action homeHandle
