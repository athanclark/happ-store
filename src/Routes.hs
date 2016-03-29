{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Routes where

import Imports
import Pages.Home
import Pages.NotFound
import Templates.Master


routes :: ( MonadApp m
          ) => HandlerT (MiddlewareT m) sec m ()
routes = do
  matchHere (action homeHandle)
  matchAny (action notFoundHandle)
  where
    homeHandle :: MonadApp m => ActionT m ()
    homeHandle = get $ html (Just AppHome) homePage
    notFoundHandle :: MonadApp m => ActionT m ()
    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("
