{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  #-}

module Handlers.Chunks where

import Imports
import qualified Data.Text as T

import Data.Attoparsec.Text (takeWhile1)
import Control.Concurrent (threadDelay)


wordChunk :: EitherUrlChunk ('Just T.Text)
wordChunk = pred_ "word" Just


delay :: IO ()
delay = threadDelay 1000000
