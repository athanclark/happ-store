{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Session where

import Imports

import Control.Applicative
import Control.Error
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Monad.Reader
import Data.TimeMap as TM
import Data.Monoid

import qualified Data.Text as T
import Data.Aeson as A hiding (json)
import Crypto.Hash
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.UTF8   as BSU8
import Data.ByteArray (convert)
import Data.UUID.V4 as UUID
import Data.UUID as UUID




data SessionRequest a = SessionRequest
  { sessionNonce :: UUID
  , sessionData  :: a
  , sessionHash  :: Hashed
  } deriving (Show, Eq)

instance FromJSON a => FromJSON (SessionRequest a) where
  parseJSON (Object o) = do
    n <- fromMaybeA $ UUID.fromString <$> o .: "nonce"
    d <- o .: "data"
    h <- fromMaybeA $ (hush . BS64.decode . BSU8.fromString) <$> o .: "hash"
    return $ SessionRequest n d h
    where
      fromMaybeA :: (Alternative f, Monad f) => f (Maybe a) -> f a
      fromMaybeA mxs = do
        mx <- mxs
        case mx of
          Nothing -> empty
          Just x  -> pure x
  parseJSON _ = empty

instance ToJSON a => ToJSON (SessionRequest a) where
  toJSON ls =
    object
      [ "nonce" .= UUID.toString (sessionNonce ls)
      , "data" .= sessionData ls
      , "hash" .= BSU8.toString (BS64.encode $ sessionHash ls)
      ]

-- For creating data for sessions that already exist
withSession :: ( MonadApp m
               , ToJSON a
               , ToJSON b
               ) => SessionRequest a
                 -> (a -> m b)
                 -> m (SessionRequest b)
withSession s f = do
  sessionCache <- envSession <$> ask
  storedHash <- liftIO $ do
    mh <- atomically $ TM.lookup (sessionNonce s) sessionCache
    case mh of
      Nothing -> throwM NonexistentNonce
      Just h  -> pure h
  let data'  = BSL.toStrict . A.encode $ sessionData s
      nonce' = BSU8.fromString . UUID.toString $ sessionNonce s
      expectedHash :: Digest SHA512
      expectedHash = hash $ nonce' <> data' <> storedHash
  if convert expectedHash /= sessionHash s
  then throwM InvalidSessionHash
  else do
    b <- f $ sessionData s
    newNonce <- liftIO UUID.nextRandom
    let b' = BSL.toStrict $ A.encode b
        newNonce' = BSU8.fromString $ UUID.toString newNonce
        newHash :: Digest SHA512
        newHash = hash $ newNonce' <> b' <> BS64.encode (convert expectedHash)
    liftIO $ do
      atomically $ TM.delete (sessionNonce s) sessionCache
      TM.insert newNonce (convert newHash) sessionCache
    pure $ SessionRequest newNonce b (convert newHash)
