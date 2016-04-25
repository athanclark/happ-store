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


loginFacebookHandle :: MonadApp m
                    => MiddlewareT m
loginFacebookHandle = action homeHandle
