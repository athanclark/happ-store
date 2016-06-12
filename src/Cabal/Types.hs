{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module Cabal.Types where

import Data.Aeson
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Hashable
import Data.Attoparsec.Text as A
import Data.List (intercalate)
import Control.Applicative



newtype Version = Version
  { getVersion :: [Int]
  } deriving (Eq, Ord, Hashable)

instance Show Version where
  show (Version vs) = intercalate "." (show <$> vs)

-- Partial, may throw exception
parseVersion :: T.Text -> Maybe Version
parseVersion t = Version <$> mapM (readMaybe . T.unpack) (T.splitOn "." t)

instance FromJSON Version where
  parseJSON (String s) =
    case parseVersion s of
      Nothing -> fail "Not in the right format"
      Just v  -> pure v
  parseJSON _ = fail "Not a string"

parseVersionA :: Parser Version
parseVersionA = do
  xs <- (A.many1 A.digit <?> "Int") `A.sepBy1` (A.char '.' <?> "Separator")
  case mapM readMaybe xs of
    Nothing -> empty
    Just ys -> pure $ Version ys


type PackageName = T.Text
