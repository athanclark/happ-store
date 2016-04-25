{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Submit where

import Handlers.Chunks

import Imports
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)



submitStatementsSearchHandle :: MonadApp m
                              => T.Text
                              -> MiddlewareT m
submitStatementsSearchHandle x = action $ do
  xs <- liftIO delay
  get $ do
    json $
      case xs of
        () -> [ ("statement1", "id1")
              , ("statement2", "id2")
              ] :: [(T.Text, T.Text)]

submitObjectsSearchHandle :: MonadApp m
                           => T.Text
                           -> MiddlewareT m
submitObjectsSearchHandle x = action $ do
  xs <- liftIO delay
  get $ do
    json $
      case xs of
        () -> [ ("object1", "id1")
              , ("object2", "id2")
              ] :: [(T.Text, T.Text)]

submitEventsNameSearchHandle :: MonadApp m
                              => T.Text
                              -> MiddlewareT m
submitEventsNameSearchHandle x = action $ do
  xs <- liftIO delay
  get $ do
    json $
      case xs of
        () -> [ ("event1", "id1")
              , ("event2", "id2")
              ] :: [(T.Text, T.Text)]

submitEventsTimeSearchHandle :: MonadApp m
                              => T.Text
                              -> MiddlewareT m
submitEventsTimeSearchHandle x = action $ do
  xs <- liftIO delay
  get $ do
    json $
      case xs of
        () -> [ ("time1", "id1")
              , ("time2", "id2")
              ] :: [(T.Text, T.Text)]


data PeopleType
  = PersonKnown
  | PersonUnknown
  | PeopleGroup

submitPeopleSearchHandle :: MonadApp m
                          => PeopleType
                          -> T.Text
                          -> MiddlewareT m
submitPeopleSearchHandle mode x =
  case mode of
    PeopleGroup -> action $ do
      xs <- liftIO delay
      get $
        json $
          case xs of
            () -> [ ("group1", "id1")
                  , ("group2", "id2")
                  ] :: [(T.Text, T.Text)]
    _ -> action $ do
      xs <- liftIO delay
      get $
        json $
          case xs of
            () -> [ ("person1", "id1")
                  , ("person2", "id2")
                  ] :: [(T.Text, T.Text)]
