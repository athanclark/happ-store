{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  #-}

module Routes where

import Imports
import Pages.Home
import Pages.NotFound
import Templates.Master
import qualified Data.Text as T
import Data.Attoparsec.Text (takeWhile1)

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)


routes :: ( MonadApp m
          ) => HandlerT (MiddlewareT m) sec m ()
routes = do
  matchHere (action homeHandle)
  matchGroup (l_ "search" </> o_) $ do
    match (l_ "statements" </> wordChunk </> o_)
      statementsSearchHandle
  matchAny (action notFoundHandle)
  where
    homeHandle :: MonadApp m => ActionT m ()
    homeHandle = get $ html (Just AppHome) homePage

    notFoundHandle :: MonadApp m => ActionT m ()
    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("

    statementsSearchHandle :: MonadApp m
                           => T.Text
                           -> MiddlewareT m
    statementsSearchHandle x = action $ do
      xs <- liftIO (threadDelay 1000000)
      get $ do
        json $
          [("Whut" :: T.Text)]
          ++ case xs of
               () -> ["yayuh"]


    wordChunk :: EitherUrlChunk ('Just T.Text)
    wordChunk = p_ "word" . takeWhile1 $ const True
