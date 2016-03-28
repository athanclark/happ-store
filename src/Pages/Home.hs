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
homePage = do
  homeLink <- T.pack <$> lift (locUrl =<< toLocation AppHome)
  a_ [href_ homeLink] "Home page"
