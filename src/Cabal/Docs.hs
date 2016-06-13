{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Cabal.Docs where

import Cabal.Types
import Imports hiding (requestHeaders)

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Client
import Data.HashMap.Strict as HM hiding (map, foldr, filter, null)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Control.Monad.Catch
import Control.Monad.Reader



parsePackageNV :: T.Text -> (PackageName, [Version])
parsePackageNV s =
  let (vs,n) = T.span (\x -> isDigit x || x == '.') $ T.reverse s
  in  ( PackageName . T.dropEnd 1 . T.reverse $ n
      , [fromJust . parseVersion . T.reverse $ vs]
      )

fetchDocs :: Env -> IO (HashMap PackageName Version)
fetchDocs env = do
  let manager = envManager env
  request <- parseUrl "https://hackage.haskell.org/packages/docs"
  let req = request { requestHeaders = [("Accept","application/json")] }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Nothing -> throwM . DocsNoParse . responseBody $ response
    Just xs ->
      pure . HM.mapMaybe (\vs -> if null vs
                                 then Nothing
                                 else Just $ maximum vs)
           . foldr (uncurry $ HM.insertWith (++)) HM.empty
           $ map (\(n,_) -> parsePackageNV n) (xs :: [(T.Text, Bool)])
