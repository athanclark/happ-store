{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , DeriveGeneric
  #-}

module Application where

import Imports hiding ((</>))
import Routes

import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Catch
import System.Directory
import GHC.Generics


data AuthRole = NeedsLogin

data AuthErr = NeedsAuth
  deriving (Generic, Show, Eq)

instance Exception AuthErr


authorize :: ( MonadThrow m
             ) => Request -> [AuthRole] -> m ()
-- authorize _ _ = return id -- uncomment to force constant authorization
authorize req ss | null ss   = return ()
                 | otherwise = throwM NeedsAuth

securityLayer :: MonadApp m => MiddlewareT m
securityLayer app req resp = do
  extractAuth authorize req routes
  app req resp

contentLayer :: MonadApp m => MiddlewareT m
contentLayer = route routes

staticLayer :: MonadApp m => MiddlewareT m
staticLayer app req respond = do
    let fileRequested = T.unpack
                      . T.intercalate "/"
                      $ pathInfo req
    basePath <- envStatic <$> ask
    let file = basePath ++ "/" ++ fileRequested
    fileExists <- liftIO (doesFileExist file)
    if fileExists
    then respond $ responseFile status200 [] file Nothing
    else app req respond

defApp :: Application
defApp _ respond =
  respond $ textOnly "Not Found! :(" status404 []
