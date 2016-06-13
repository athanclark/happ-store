{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

module Cabal.Uploaded where

import Cabal.Types
import Imports hiding (requestHeaders)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Data.Time
import Control.Monad.Catch
import Control.Monad.Reader



parseUploadTime :: T.Text -> Maybe UTCTime
parseUploadTime t =
  let [_,m,d,ti,_,y] = T.words t
      year = read $ T.unpack y
      day  = read $ T.unpack d
      mon | m == "Jan" = 1
          | m == "Feb" = 2
          | m == "Mar" = 3
          | m == "Apr" = 4
          | m == "May" = 5
          | m == "Jun" = 6
          | m == "Jul" = 7
          | m == "Aug" = 8
          | m == "Sep" = 9
          | m == "Oct" = 10
          | m == "Nov" = 11
          | m == "Dec" = 12
      [h,mi,s] = T.splitOn ":" ti
      hour = read $ T.unpack h
      min  = read $ T.unpack mi
      sec  = read $ T.unpack s
  in  Just UTCTime
        { utctDay = fromGregorian year mon day
        , utctDayTime = secondsToDiffTime $
                          (hour * 3600) + (min * 60) + sec
        }

fetchUploadTime :: Env -> PackageName -> Version -> IO (Maybe UTCTime)
fetchUploadTime env (PackageName package) version = do
  let manager = envManager env
      url = "https://hackage.haskell.org/package/"
         ++ T.unpack package ++ "-" ++ show version ++ "/upload-time"
  request  <- parseUrl url
  response <- httpLbs request manager
  let t = T.decodeUtf8 . LBS.toStrict . responseBody $ response
  pure (parseUploadTime t) `catch` (\(_ :: SomeException) -> pure Nothing)
