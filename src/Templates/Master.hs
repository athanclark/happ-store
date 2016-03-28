{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Templates.Master where

import Imports hiding (FileExt (..))

import qualified Data.Text as T
import Web.Page.Lucid
import Lucid
import Data.Markup
import Data.Url
import Path.Extended
import qualified Network.Wai.Middleware.ContentType.Types as CT

import Data.Monoid
import Data.Default
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State (modify)
import Control.Monad.Morph (hoist)
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
  bytestring CT.Html bs
  modify $ HM.map $ mapStatus (const s)
                  . mapHeaders (("Content-Type","text/html"):)

-- | Shortcut for rendering with a template
html :: ( MonadApp m
        ) => Maybe AppLinks
          -> HtmlT (AppTemplateT m) ()
          -> FileExtListenerT m ()
html state content = htmlLight status200 (mainTemplate state content)


-- * Templates

masterPage :: MonadApp m => WebPage (HtmlT m ()) T.Text
masterPage =
  let page :: MonadApp m => WebPage (HtmlT m ()) T.Text
      page = def
  in  page { pageTitle = "Cooperate"
           , afterStylesScripts = do
               scriptAssets
               elmScripts
           , styles = do
               styleAssets
               lessStyles
           }
  where
    scriptAssets :: MonadApp m => HtmlT m ()
    scriptAssets = do
      hostname <- envAuthority <$> lift ask
      isProd <- envProduction <$> lift ask
      if isProd
      then hoist (`runAbsoluteUrlT` cloudflareCdn) $ do
             jQuery <- lift (toLocation JQueryCdn)
             deploy JavaScript Remote jQuery
             semantic <- lift (toLocation SemanticJsCdn)
             deploy JavaScript Remote semantic

      else hoist (`runAbsoluteUrlT` hostname) $ do
             jQuery <- lift (toLocation JQuery)
             deploy JavaScript Remote jQuery
             semantic <- lift (toLocation SemanticJs)
             deploy JavaScript Remote semantic

    elmScripts :: MonadApp m => HtmlT m ()
    elmScripts = do
      hostname <- envAuthority <$> lift ask
      hoist (`runAbsoluteUrlT` hostname) $ do
        app <- lift (toLocation AppFrontend)
        deploy JavaScript Remote app
      deploy JavaScript Inline
        ("var app = Elm.fullscreen(Elm.Bingo);" :: T.Text)

    styleAssets :: MonadApp m => HtmlT m ()
    styleAssets = do
      hostname <- envAuthority <$> lift ask
      isProd <- envProduction <$> lift ask
      if isProd
      then hoist (`runAbsoluteUrlT` cloudflareCdn) $ do
             semantic <- lift (toLocation SemanticCssCdn)
             deploy Css Remote semantic
      else hoist (`runAbsoluteUrlT` hostname) $ do
             semantic <- lift (toLocation SemanticCss)
             deploy Css Remote semantic

    lessStyles :: MonadApp m => HtmlT m ()
    lessStyles = do
      hostname <- envAuthority <$> lift ask
      hoist (`runAbsoluteUrlT` hostname) $ do
        lessStyles <- lift (toLocation LessStyles)
        deploy Css Remote lessStyles

    cloudflareCdn = UrlAuthority "https" True Nothing "cdnjs.cloudflare.com" Nothing

masterTemplate :: ( Monad m
                  ) => Maybe AppLinks
                    -> WebPage (HtmlT m ()) T.Text
                    -> HtmlT m ()
                    -> HtmlT m ()
masterTemplate _ = template

mainTemplate :: ( MonadApp m
                ) => Maybe AppLinks
                  -> HtmlT m ()
                  -> HtmlT m ()
mainTemplate state = masterTemplate state masterPage


-- * Utilities

appendTitle :: WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page { pageTitle = x <> " - " <> pageTitle page }

