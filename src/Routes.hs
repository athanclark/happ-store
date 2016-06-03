{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  #-}

module Routes where

import Imports
import Handlers
import qualified Data.Text as T



routes :: ( MonadApp m
          ) => HandlerT (MiddlewareT m) sec m ()
routes = do
  matchHere (action homeHandle)
  matchGroup (l_ "browse" </> o_) $ do
    matchHere (action homeHandle)
    match (l_ "view" </> wordChunk </> o_)
      browseViewHandle
    match (l_ "search" </> wordChunk </> o_)
      browseSearchHandle
  matchGroup (l_ "people" </> o_) $ do
    match (l_ "search" </> wordChunk </> o_)
      peopleSearchHandle
    match (l_ "view" </> wordChunk </> o_)
      peopleViewHandle
  matchGroup (l_ "login" </> o_) $ do
    matchHere loginHandle
    match (l_ "facebook" </> o_)
      loginFacebookHandle
  matchGroup (l_ "session" </> o_) $
    matchHere sessionHandle
  matchAny (action notFoundHandle)
