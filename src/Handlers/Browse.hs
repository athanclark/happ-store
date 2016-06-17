{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Browse where

import Handlers.Chunks
import Handlers.App
import Schema
import Cabal.Types

import Imports
import Data.Acid.Advanced (query')
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader


browseSearchHandle :: MonadApp m
                   => T.Text
                   -> MiddlewareT m
browseSearchHandle x = action $ do
  xs <- liftIO delay
  get $ do
    json $
      case xs of
        () -> [ ("foo" :: T.Text)
              ]

packagesHandle :: MonadApp m
               => T.Text
               -> MiddlewareT m
packagesHandle packageName app req respond = do
  db <- envDatabase <$> ask
  mPackage <- query' db . LookupPackage . PackageName $ packageName
  let handle = action $
        get $
          case mPackage of
            Nothing -> text ("Not found!" :: LT.Text)
            Just p  -> json p
  handle app req respond


allPackagesHandle :: MonadApp m
                  => MiddlewareT m
allPackagesHandle app req respond = do
  db <- envDatabase <$> ask
  vs <- query' db CurrentKnownPackageVersions
  let handle = action $
        get $
          json vs
  handle app req respond

browseViewHandle :: MonadApp m
                 => T.Text
                 -> MiddlewareT m
browseViewHandle x = action homeHandle
  -- FIXME: Populate state someherw :\
