{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Pages.NotFound where

import Application.Types
import Templates.Master

import Data.Url
import Path.Extended
import Web.Page.Lucid
import qualified Data.Text as T
import Lucid
import Lucid.Base

import Data.Monoid
import Control.Monad.Trans


notFoundContent :: ( MonadApp m
                   ) => HtmlT m ()
notFoundContent = do
  home <- T.pack <$> lift (locUrl =<< toLocation AppHome)

  let page :: Monad m => WebPage (HtmlT m ()) T.Text
      page = masterPage { metaVars = meta_ [ makeAttribute "http-equiv" "refresh"
                                           , content_ $ "3;url=" <> home
                                           ]
                        }

  template page $
    div_ [] $ do
      h1_ [] "Not Found!"
      p_ [] $ do
        "Your request was not found. Redirecting you to "
        a_ [href_ home] "the homepage"
        "..."


