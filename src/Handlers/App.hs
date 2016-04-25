{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module Handlers.App where


import Imports
import qualified Data.Text as T
import Pages.Home
import Pages.NotFound
import Templates.Master


homeHandle :: MonadApp m => ActionT m ()
homeHandle = get $ html (Just AppHome) homePage

notFoundHandle :: MonadApp m => ActionT m ()
notFoundHandle = get $ do
  htmlLight status404 notFoundContent
  text "404 :("
