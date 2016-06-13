{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , FlexibleContexts
  #-}

module Cabal.Preferred where

import Cabal.Types
import Imports

import Data.Attoparsec.Text.Lazy as A
import qualified Data.Text  as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.HTTP.Client

import Data.Foldable
import Data.Char (digitToInt)
import Data.HashMap.Lazy as HM hiding (empty)
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import GHC.Generics


satisfies :: [Int] -> Preferred -> Bool
satisfies = any . satisfiesConstraint
  where
    satisfiesConstraint :: [Int] -> Constraint -> Bool
    satisfiesConstraint ns c =
      case c of
        EqualTo es                    -> satisfiesEqual ns es
        Between g ls us               -> satisfiesBetween ns (g,ls) us
        Above g ls | g == GreaterThan -> satisfiesGreaterThan ns ls
                   | otherwise        -> satisfiesGreaterThanEq ns ls
        Below us                      -> satisfiesBelow ns us

    satisfiesEqual :: [Int] -> EqVersion -> Bool
    satisfiesEqual [] Nil = True
    satisfiesEqual (n:ns) (Cons x xs)
      | n == x    = satisfiesEqual ns xs
      | otherwise = False
    satisfiesEqual _ Wildcard = True
    satisfiesEqual _ _        = False

    satisfiesBelow :: [Int] -> [Int] -> Bool
    satisfiesBelow [] [] = False -- they were the same
    satisfiesBelow (n:ns) (x:xs)
      | n < x     = True
      | n == x    = satisfiesBelow ns xs
      | otherwise = False
    satisfiesBelow (_:_) [] = False -- more specific, and thus greater
    satisfiesBelow [] (_:_) = True

    satisfiesGreaterThan :: [Int] -> [Int] -> Bool
    satisfiesGreaterThan [] [] = False -- they were the same
    satisfiesGreaterThan (n:ns) (x:xs)
      | n > x     = True
      | n == x    = satisfiesGreaterThan ns xs
      | otherwise = False
    satisfiesGreaterThan (_:_) [] = True
    satisfiesGreaterThan [] (_:_) = False

    satisfiesGreaterThanEq :: [Int] -> [Int] -> Bool
    satisfiesGreaterThanEq [] [] = True
    satisfiesGreaterThanEq (n:ns) (x:xs)
      | n > x     = True
      | n == x    = satisfiesGreaterThanEq ns xs
      | otherwise = False
    satisfiesGreaterThanEq (_:_) [] = True
    satisfiesGreaterThanEq [] (_:_) = False

    satisfiesBetween :: [Int] -> (Greater, [Int]) -> [Int] -> Bool
    satisfiesBetween ns (g,ls) us =
      satisfiesBelow ns us && (case g of
                                 GreaterThan   -> satisfiesGreaterThan ns ls
                                 GreaterThanEq -> satisfiesGreaterThanEq ns ls)


parsePreferred :: Parser Preferred
parsePreferred =
  (A.skipSpace >> parseConstraint) `A.sepBy1` (A.skipSpace >> parseOr)
  where
    parseConstraint :: Parser Constraint
    parseConstraint =
          EqualTo <$> parseEqualV
      <|> Below <$> parseLessV
      <|> (\(g,l,u) -> Between g l u) <$> parseBetween
      <|> Above <$> parseGreater <*> parseExactV

    parseEqualV :: Parser EqVersion
    parseEqualV = do
      parseEq
      xs <- (digOrWild `A.sepBy1` A.char '.') <?> "Wildcard Version"
      let go :: Either Int Char -> EqVersion -> Parser EqVersion
          go (Right _) Nil = pure Wildcard
          go (Right _) _   = empty
          go (Left d) xs'  = pure $ Cons d xs'
      foldrM go Nil xs
      where
        parseEq :: Parser T.Text
        parseEq = A.string "==" <?> "Equal"

        digOrWild :: Parser (Either Int Char)
        digOrWild = (Left . read) <$> A.many1 A.digit
                <|> Right         <$> A.char '*'

    parseBetween :: Parser (Greater, [Int], [Int])
    parseBetween = do
      A.skipSpace
      g <- parseGreater
      l <- parseExactV
      A.skipSpace
      parseAnd
      A.skipSpace
      u <- parseLessV
      pure (g,l,u)

    parseGreater :: Parser Greater
    parseGreater = GreaterThanEq <$ parseGq
               <|> GreaterThan   <$ parseGt
      where
        parseGt :: Parser T.Text
        parseGt = A.string ">" <?> "GT"

        parseGq :: Parser T.Text
        parseGq = A.string ">=" <?> "GEQ"

    parseExactV :: Parser [Int]
    parseExactV = do
      xs <- (A.many1 A.digit `A.sepBy1` char '.') <?> "Exact Version"
      pure (read <$> xs)

    parseLessV :: Parser [Int]
    parseLessV = do
      parseLt
      parseExactV
      where
        parseLt :: Parser T.Text
        parseLt = A.string "<" <?> "LT"

    parseAnd :: Parser T.Text
    parseAnd = A.string "&&" <?> "And"

    parseOr :: Parser T.Text
    parseOr = A.string "||" <?> "Or"



fetchPreferred :: MonadApp m => m (HashMap PackageName Preferred)
fetchPreferred = do
  manager  <- envManager <$> ask
  request  <- parseUrl "https://hackage.haskell.org/packages/preferred-versions"
  response <- liftIO $ httpLbs request manager
  let xs = dropWhile ("--" `LT.isPrefixOf`)
         . LT.lines . LT.decodeUtf8 . responseBody $ response
      startingPref c = c == '<' || c == '>' || c == '='
      parsePref p = case A.parse parsePreferred p of
                      Done _ r -> pure r
                      e        -> throwM . ParseError . show $ e
      nameAndPrefs :: MonadApp m => LT.Text -> m (PackageName, Preferred)
      nameAndPrefs l = do
        let (n,p) = LT.span (not . startingPref) l
        p' <- parsePref $ LT.strip p
        pure ( PackageName . T.strip . LT.toStrict $ n
             , p'
             )
  ys <- mapM nameAndPrefs xs
  pure $ HM.fromList ys
