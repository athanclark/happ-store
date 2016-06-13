{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Session where

import Imports

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Data.TimeMap as TM

import qualified Data.Text.Encoding as T
import Data.Aeson as A hiding (json)
import qualified Crypto.Saltine.Core.Sign as NaCl
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Base16 as BS16



-- * Signatures

newtype Signature = Signature
  { getSignature :: BS.ByteString
  } deriving (Show, Eq)

instance FromJSON Signature where
  parseJSON (String s) =
    case BS16.decode $ T.encodeUtf8 s of
      (decoded, rest) | rest /= "" -> fail "Not base-16 encoded"
                      | otherwise  -> pure $ Signature decoded
  parseJSON _ = fail "Not a string"

instance ToJSON Signature where
  toJSON (Signature s) =
    toJSON . T.decodeUtf8 . BS16.encode $ s


-- * Requests

data SignedRequest = SignedRequest
  { sessionPublicKey :: ClientPublicKey
  , sessionSignature :: Signature
  } deriving (Show, Eq)

instance FromJSON SignedRequest where
  parseJSON (Object o) =
    SignedRequest <$> (o .: "publicKey") <*> (o .: "signature")
  parseJSON _ = empty


-- * Signing and Verification

sign :: ( ToJSON a
        , MonadApp m
        ) => a -> m Signature
sign x = do
  sk <- envSecretKey <$> ask
  pure . Signature . NaCl.sign sk . LBS.toStrict . A.encode $ x


verify :: ( FromJSON a
          ) => SignedRequest -> Maybe a
verify (SignedRequest pk (Signature s)) = do
  pk'     <- toNaClPublicKey pk
  message <- NaCl.signOpen pk' s
  A.decodeStrict message


withSession :: ( MonadApp m
               , FromJSON a
               , ToJSON b
               ) => (a -> m b)
                 -> SignedRequest
                 -> m Signature
withSession f req@(SignedRequest sId sig) =
  case verify req of
    Nothing -> throwM $ InvalidSignedRequest $ show req
    Just x  -> do
      -- update the session id
      sessionCache <- envSession <$> ask
      liftIO $ TM.touch sId sessionCache
      sign =<< f x

 -- TODO: Authentication; for each route? Some kind of UserId -> Credentials doodad?
 -- sessionCache <- envSession <$> ask
 -- storedUserId <- do
 --   mh <- liftIO . atomically $ TM.lookup sId sessionCache
 --   case mh of
 --     Nothing -> throwM (NonexistentSessionId sId)
 --     Just h  -> pure h
