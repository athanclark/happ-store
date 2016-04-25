{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.People where

import Handlers.Chunks
import Handlers.App

import Imports
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)



peopleSearchHandle :: MonadApp m
                   => T.Text
                   -> MiddlewareT m
peopleSearchHandle x = action $ do
  xs <- liftIO delay
  get $
    json $
      case xs of
        () -> [ ("merp" :: T.Text)
              ]

peopleViewHandle :: MonadApp m
                 => T.Text
                 -> MiddlewareT m
peopleViewHandle x = action homeHandle
