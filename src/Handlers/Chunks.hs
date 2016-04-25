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
wordChunk = p_ "word" . takeWhile1 $ const True


delay :: IO ()
delay = threadDelay 1000000
