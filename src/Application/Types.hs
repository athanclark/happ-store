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
  { envAuthority :: UrlAuthority
  , envCwd       :: FilePath -- ^ for File Processing
  , envStatic    :: FilePath
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


appendActiveWhen :: AppLinks -> Maybe AppLinks -> T.Text -> T.Text
appendActiveWhen x (Just y) c | x == y = c <> " active"
appendActiveWhen _ _ c = c
