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
  matchGroup (l_ "submit" </> o_) $ do
    matchHere (action homeHandle)
    matchGroup (l_ "search" </> o_) $ do
      match (l_ "statements" </> wordChunk </> o_)
        submitStatementsSearchHandle
      match (l_ "objects" </> wordChunk </> o_)
        submitObjectsSearchHandle
      matchGroup (l_ "events" </> o_) $ do
        match (wordChunk </> o_)
          submitEventsNameSearchHandle
        match (l_ "time" </> wordChunk </> o_)
          submitEventsTimeSearchHandle
      matchGroup (l_ "people" </> o_) $ do
        match (l_ "known" </> wordChunk </> o_)
          (submitPeopleSearchHandle PersonKnown)
        match (l_ "unknown" </> wordChunk </> o_)
          (submitPeopleSearchHandle PersonUnknown)
        match (l_ "group" </> wordChunk </> o_)
          (submitPeopleSearchHandle PeopleGroup)
  matchGroup (l_ "people" </> o_) $ do
    match (l_ "search" </> wordChunk </> o_)
      peopleSearchHandle
    match (l_ "view" </> wordChunk </> o_)
      peopleViewHandle
  matchGroup (l_ "login" </> o_) $ do
    match (l_ "facebook" </> o_)
      loginFacebookHandle
  matchAny (action notFoundHandle)
