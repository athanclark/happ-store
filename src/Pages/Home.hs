{-# LANGUAGE
    OverloadedStrings
  , ExtendedDefaultRules
  , FlexibleContexts
  #-}

module Pages.Home where

import Application.Types
import Data.Url
import Path.Extended
import Lucid
import qualified Data.Text as T

import Control.Monad.Trans


homePage :: ( MonadApp m
            ) => HtmlT m ()
homePage = return ()
