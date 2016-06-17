{-# LANGUAGE
    FlexibleContexts
  #-}

module Network.Wai.ContentType.Extended where

import Application.Types
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Network.Wai.Middleware.ContentType as CT
import Data.Aeson
import Lucid
import Control.Concurrent.QSem



lucid :: ( MonadApp m
         ) => HtmlT m () -> CT.FileExtListenerT m ()
lucid x = do
  lucidQueue <- (queueHtml . envQueues) <$> ask
  bracket_ (liftIO $ waitQSem lucidQueue) (liftIO $ signalQSem lucidQueue) $
    CT.lucid x

json :: ( MonadApp m
        , ToJSON a
        ) => a -> CT.FileExtListenerT m ()
json x = do
  jsonQueue <- (queueJson . envQueues) <$> ask
  bracket_ (liftIO $ waitQSem jsonQueue) (liftIO $ signalQSem jsonQueue) $
    CT.json x
