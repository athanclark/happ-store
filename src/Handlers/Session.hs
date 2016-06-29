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

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as LT
import qualified Data.ByteString.Base16 as BS16
import Data.Aeson as A hiding (json)
import Network.WebSockets
import Crypto.Saltine.Class as NaCl

import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Monad.Reader



sessionHandle :: MiddlewareT AppM
sessionHandle app req respond =
  let handleHTTP :: MonadApp m => MiddlewareT m
      handleHTTP = action $
        post $ do
          let f :: MonadApp m => T.Text -> m T.Text
              f x = if x == "ping"
                    then pure "pong"
                    else throwM BadSessionFormat
          mx <- liftIO $ A.decode <$> strictRequestBody req
          y  <- case mx of
                  Nothing -> throwM BadSessionFormat
                  Just x  -> withSession f x
          json y

      handleWS :: MiddlewareT AppM
      handleWS app req resp = do
        env <- ask
        let ws = websocketsOrT (`runAppM` env) defaultConnectionOptions
               $ \pendingConn -> do
                   pk <- T.decodeUtf8 . BS16.encode . NaCl.encode . envPublicKey <$> ask
                   liftIO $ do
                     conn <- acceptRequest pendingConn
                     sendTextData conn pk
                     () <$ receiveDataMessage conn
        ws app req resp

      handle = handleWS . handleHTTP
  in  (handle `catchMiddlewareT` errorCatcher) app req respond


errorCatcher :: MonadApp m
             => SessionException -> MiddlewareT m
errorCatcher e app req respond =
  case e of
    InvalidSignedRequest s -> do
      liftIO . putStrLn $ "Invalid Signed Request! " ++ s
      respond $ textOnly ("Invalid Signed Request! " <> LT.pack s) status403 []
    BadSessionFormat ->
      respond $ textOnly "Malformed Data" status400 []
    NonexistentSessionId s -> do
      liftIO . putStrLn $ "Session Id not found: " ++ show s
      respond $ textOnly ("Session Id not found: " <> LT.pack (show s)) status404 []
