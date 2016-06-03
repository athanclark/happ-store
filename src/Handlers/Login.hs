{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Handlers.Login where

import Handlers.Chunks
import Handlers.App

import Imports
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Applicative
import Control.Error
import Data.Monoid

import Data.Aeson as A hiding (json)
import Crypto.Hash
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.UTF8   as BSU8
import Data.ByteArray (convert)
import Data.UUID.V4
import Data.UUID as UUID


fromMaybeA :: (Alternative f, Monad f) => f (Maybe a) -> f a
fromMaybeA mxs = do
  mx <- mxs
  case mx of
    Nothing -> empty
    Just x  -> pure x


data LoginCredentials = LoginPlain
      { username :: String
      , password :: String
      }
  deriving (Show, Eq)

instance FromJSON LoginCredentials where
  parseJSON (Object o) = do
    u <- o .: "username"
    p <- o .: "password"
    return $ LoginPlain u p
  parseJSON _ = empty

instance ToJSON LoginCredentials where
  toJSON ls =
    object
      [ "username" .= username ls
      , "password" .= password ls
      ]

data LoginRequest = LoginRequest
  { loginNonce       :: UUID
  , loginCredentials :: LoginCredentials
  , loginHash        :: BS.ByteString
  } deriving (Show, Eq)

instance FromJSON LoginRequest where
  parseJSON (Object o) = do
    n  <- fromMaybeA $ UUID.fromString <$> o .: "nonce"
    cs <- parseJSON . Object =<< o .: "data"
    h  <- fromMaybeA $ (hush . BS64.decode . BSU8.fromString) <$> o .: "hash"
    return $ LoginRequest n cs h
  parseJSON _ = empty

instance ToJSON LoginRequest where
  toJSON ls =
    object
      [ "nonce" .= UUID.toString (loginNonce ls)
      , "data" .= loginCredentials ls
      , "hash" .= BSU8.toString (BS64.encode $ loginHash ls)
      ]

checkLoginRequest :: LoginRequest -> Bool
checkLoginRequest l =
  let data' = BSL.toStrict . A.encode $ loginCredentials l
      nonce = BSU8.fromString . UUID.toString $ loginNonce l
      hash' :: Digest SHA512
      hash'  = hash $ nonce <> data'
  in  convert hash' == loginHash l

loginHandle :: MonadApp m
            => MiddlewareT m
loginHandle = action $ do
  post uploader $ json ("ayyy" :: T.Text)
  where
    uploader :: MonadApp m => Request -> m ()
    uploader r = do
      liftIO $ do
        ml <- A.decode <$> strictRequestBody r :: IO (Maybe LoginRequest)
        case ml of
          Nothing -> throwM BadLoginData
          Just l -> do
            putStrLn $ "Login Request:\n" ++ show l
            putStrLn $ "Is Legit: " ++ show (checkLoginRequest l)
      return ()

loginFacebookHandle :: MonadApp m
                    => MiddlewareT m
loginFacebookHandle = action homeHandle
