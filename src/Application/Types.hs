{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module Application.Types where

import Path.Extended
import qualified Data.Text as T
import Data.Monoid
import Data.Url
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Catch


type AppM a = LoggingT (ReaderT Env IO) a

runAppT :: AppM a -> Env -> IO a
runAppT hs = runReaderT $ runStderrLoggingT hs

type AppTemplateT m = AbsoluteUrlT m

runAppTemplateT :: AppTemplateT m a -> UrlAuthority -> m a
runAppTemplateT = runAbsoluteUrlT


type MonadApp m =
  ( MonadIO m
  , MonadThrow m
  , MonadLogger m
  , MonadReader Env m
  , MonadUrl Abs File m
  , MonadBaseControl IO m
  )


-- The environment accessible from our application
data Env = Env
  { envAuthority  :: UrlAuthority
  , envCwd        :: FilePath -- ^ for File Processing
  , envStatic     :: FilePath
  , envProduction :: Bool
  } deriving (Show, Eq)

-- | Data type representing top navigation bar
data AppLinks
  = AppHome
  | AppAbout
  | AppContact
  deriving (Show, Eq)

instance ToPath AppLinks Abs File where
  toPath AppHome    = parseAbsFile "/index"
  toPath AppAbout   = parseAbsFile "/about"
  toPath AppContact = parseAbsFile "/contact"

instance ToLocation AppLinks Abs File where
  toLocation x = fromPath <$> toPath x

data AppResources
  = JQuery
  | JQueryCdn
  | SemanticJs
  | SemanticJsCdn
  | SemanticCss
  | SemanticCssCdn
  | RangeSliderJs
  | RangeSliderJsCdn
  | LessStyles
  | AppFrontend
  deriving (Show, Eq)

instance ToPath AppResources Abs File where
  toPath JQuery           = parseAbsFile "/jquery"
  toPath SemanticJs       = parseAbsFile "/semantic"
  toPath SemanticCss      = parseAbsFile "/semantic"
  toPath JQueryCdn        = parseAbsFile "/ajax/libs/jquery/3.0.0-beta1/jquery"
  toPath SemanticJsCdn    = parseAbsFile "/ajax/libs/semantic-ui/2.1.8/semantic"
  toPath SemanticCssCdn   = parseAbsFile "/ajax/libs/semantic-ui/2.1.8/semantic"
  toPath RangeSliderJs    = parseAbsFile "/rangeslider"
  toPath RangeSliderJsCdn = parseAbsFile "/ajax/libs/rangeslider.js/2.1.1/rangeslider"
  toPath LessStyles       = parseAbsFile "/style"
  toPath AppFrontend      = parseAbsFile "/App"

instance ToLocation AppResources Abs File where
  toLocation JQuery           = (addFileExt "min.js"  . fromPath) <$> toPath JQuery
  toLocation SemanticJs       = (addFileExt "min.js"  . fromPath) <$> toPath SemanticJs
  toLocation SemanticCss      = (addFileExt "min.css" . fromPath) <$> toPath SemanticCss
  toLocation JQueryCdn        = (addFileExt "min.js"  . fromPath) <$> toPath JQueryCdn
  toLocation SemanticJsCdn    = (addFileExt "min.js"  . fromPath) <$> toPath SemanticJsCdn
  toLocation SemanticCssCdn   = (addFileExt "min.css" . fromPath) <$> toPath SemanticCssCdn
  toLocation RangeSliderJs    = (addFileExt "min.js"  . fromPath) <$> toPath RangeSliderJs
  toLocation RangeSliderJsCdn = (addFileExt "min.js"  . fromPath) <$> toPath RangeSliderJsCdn
  toLocation LessStyles       = (addFileExt "css"     . fromPath) <$> toPath LessStyles
  toLocation AppFrontend      = (addFileExt "js"      . fromPath) <$> toPath AppFrontend

appendActiveWhen :: AppLinks -> Maybe AppLinks -> T.Text -> T.Text
appendActiveWhen x (Just y) c | x == y = c <> " active"
appendActiveWhen _ _ c = c
