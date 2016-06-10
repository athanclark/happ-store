{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Session
  ( sessionHandle
  ) where

import Handlers.Chunks
import Handlers.App

import Session

import Imports
import Data.Monoid
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Aeson as A hiding (json)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch



sessionHandle :: MonadApp m
              => MiddlewareT m
sessionHandle app req respond =
  let handle = action $
        post $ do
          let f :: MonadApp m => T.Text -> m T.Text
              f x = if x == "ping"
                    then pure "pong"
                    else throwM BadSessionFormat
          mx <- liftIO $ A.decode <$> strictRequestBody req
          x  <- case mx of
                  Nothing -> throwM BadSessionFormat
                  Just x  -> pure x
          y <- withSession f x
          json y
  in  (handle `catchMiddlewareT` errorCatcher) app req respond


errorCatcher :: MonadApp m
             => SessionException -> MiddlewareT m
errorCatcher e app req respond =
  case e of
    InvalidSignedRequest ->
      respond $ textOnly "Invalid Signed Request!" status403 []
    BadSessionFormat ->
      respond $ textOnly "Malformed Data" status400 []
    NonexistentSessionId s -> do
      liftIO . putStrLn $ "Session Id not found: " ++ show s
      respond $ textOnly ("Session Id not found: " <> LT.pack (show s)) status404 []
