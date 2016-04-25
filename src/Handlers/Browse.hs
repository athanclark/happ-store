{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Browse where

import Handlers.Chunks
import Handlers.App

import Imports
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)


browseSearchHandle :: MonadApp m
                   => T.Text
                   -> MiddlewareT m
browseSearchHandle x = action $ do
  xs <- liftIO delay
  get $ do
    json $
      case xs of
        () -> [ ("foo" :: T.Text)
              ]

browseViewHandle :: MonadApp m
                 => T.Text
                 -> MiddlewareT m
browseViewHandle x = action homeHandle
  -- FIXME: Populate state someherw :\
