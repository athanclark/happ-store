{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Templates.Master where

import Imports

import qualified Data.Text as T
import Web.Page.Lucid
import Lucid

import Data.Monoid
import Data.Default
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State (modify)
import qualified Data.HashMap.Lazy as HM


-- * Content-Type Functions

-- | Render without @mainTemplate@
htmlLight :: ( MonadApp m
             ) => Status
               -> HtmlT (AppTemplateT m) a
               -> FileExtListenerT m ()
htmlLight s content = do
  hostname <- envAuthority <$> ask
  bs <- lift $ runAppTemplateT (renderBST content) hostname
  bytestring Html bs
  modify $ HM.map $ mapStatus (const s)
                  . mapHeaders (("Content-Type","text/html"):)

-- | Shortcut for rendering with a template
html :: ( MonadApp m
        ) => Maybe AppLinks
          -> HtmlT (AppTemplateT m) ()
          -> FileExtListenerT m ()
html state content = htmlLight status200 $ mainTemplate state content


-- * Templates

masterPage :: Monad m => WebPage (HtmlT m ()) T.Text
masterPage = def

masterTemplate :: ( Monad m
                  ) => Maybe AppLinks
                    -> WebPage (HtmlT m ()) T.Text
                    -> HtmlT m ()
                    -> HtmlT m ()
masterTemplate _ = template

mainTemplate :: ( Monad m
                ) => Maybe AppLinks
                  -> HtmlT m ()
                  -> HtmlT m ()
mainTemplate state = masterTemplate state masterPage


-- * Utilities

appendTitle :: WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page { pageTitle = x <> " - " <> pageTitle page }

