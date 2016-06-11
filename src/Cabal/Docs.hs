{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , FlexibleContexts
  #-}

module Cabal.Docs where

import Imports hiding (requestHeaders)

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Data.HashMap.Strict as HM hiding (map, foldr, filter, null)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Control.Monad.Catch
import Control.Monad.Reader

import GHC.Generics


type PackageName = T.Text
type Version = [Int]

parsePackageNV :: T.Text -> (PackageName, [Version])
parsePackageNV s =
  let (vs,n) = T.span (\x -> isDigit x || x == '.') $ T.reverse s
  in  ( T.dropEnd 1 $ T.reverse n
      , [map (read . T.unpack) $ T.splitOn "." $ T.reverse vs]
      )


data DocsError
  = DocsNoParse LBS.ByteString
  deriving (Show, Eq, Generic)

instance Exception DocsError

fetchDocs :: MonadApp m => m (HashMap PackageName (Maybe Version))
fetchDocs = do
  manager  <- envManager <$> ask
  request  <- parseUrl "https://hackage.haskell.org/packages/docs"
  let req = request
              { requestHeaders = [("Accept","application/json")] }
  response <- liftIO $ httpLbs req manager
  case decode (responseBody response) of
    Nothing -> throwM . DocsNoParse . responseBody $ response
    Just xs ->
      pure . fmap (\vs -> if null vs
                          then Nothing
                          else Just $ maximum vs)
           . foldr (uncurry $ HM.insertWith (++)) HM.empty
           $ map (\(n,_) -> parsePackageNV n) (xs :: [(T.Text, Bool)])
