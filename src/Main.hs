{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  #-}

module Main where

import Imports
import Schema
import Application

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger

import System.Directory
import GHC.Generics
import Path.Extended

import Data.Url
import Data.Maybe
import Data.Default
import Data.Monoid
import Control.Monad


-- * Options Parsing

-- | Application-wide options
data AppOpts = AppOpts
  { port   :: Maybe Int
  , host   :: Maybe String
  , cwd    :: Maybe FilePath
  , static :: Maybe FilePath
  } deriving (Show, Eq, Generic)

instance Monoid AppOpts where
  mempty = AppOpts Nothing Nothing Nothing Nothing
  mappend (AppOpts p1 h1 c1 s1) (AppOpts p2 h2 c2 s2) =
    AppOpts (getLast $ (Last p1) <> (Last p2))
            (getLast $ (Last h1) <> (Last h2))
            (getLast $ (Last c1) <> (Last c2))
            (getLast $ (Last s1) <> (Last s2))

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

instance Default AppOpts where
  def = AppOpts (Just 3000) (Just "localhost") Nothing Nothing

appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> optional ( option auto
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on - DEF: 3000" ))
  <*> optional ( strOption
        ( long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "host to deploy URLs over - DEF: 'http://localhost'" ))
  <*> optional ( strOption
        ( long "cwd"
       <> short 'c'
       <> metavar "CWD"
       <> help "absolute directory to search for configuration files - DEF: `pwd`/" ))
  <*> optional ( strOption
        ( long "static"
       <> short 's'
       <> metavar "STATIC"
       <> help "absolute directory to search for servable static files - DEF: `pwd`/static/" ))

-- | Command-line options
data App = App
  { options :: AppOpts
  , configPath :: Maybe String
  } deriving (Show, Eq)

app :: Parser App
app = App
  <$> appOpts
  <*> optional ( strOption
        ( long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> help "absolute path to config file - DEF: `pwd`/config/app.yaml" ))


-- * Executable

main :: IO ()
main = do
  (commandOpts :: App) <- execParser opts
  cwd' <- getCurrentDirectory
  let yamlConfigPath' = fromMaybe (cwd' <> "/config/app.yaml") $
        configPath commandOpts

  yamlConfigPath     <- toFilePath <$> parseAbsFile yamlConfigPath'
  yamlConfigExists   <- doesFileExist yamlConfigPath
  yamlConfigContents <- if yamlConfigExists
                        then readFile yamlConfigPath
                        else return ""
  mYamlConfig <- if yamlConfigExists && yamlConfigContents /= ""
                 then Y.decodeFile yamlConfigPath
                 else return Nothing

  let yamlConfig = fromMaybe def mYamlConfig
      dirs = (AppOpts Nothing Nothing (Just $ cwd' <> "/") (Just $ cwd' <> "/static/"))
      config = dirs <> def <> yamlConfig <> options commandOpts

  cwdDir    <- toFilePath <$> parseAbsDir (fromJust $ cwd config)
  staticDir <- toFilePath <$> parseAbsDir (fromJust $ static config)

  cwdExists <- doesDirectoryExist cwdDir
  staticExists <- doesDirectoryExist staticDir

  case (cwdExists,staticExists) of
    (False,_) -> error $ "--cwd argument `"    <> cwdDir    <> "` does not exist"
    (_,False) -> error $ "--static argument `" <> staticDir <> "` does not exist"
    _ -> entry (fromJust $ port config) $ appOptsToEnv config
  where
    opts :: ParserInfo App
    opts = info (helper <*> app)
      ( fullDesc
     <> progDesc "Serve application from PORT over HOST"
     <> header "compatable-opinions - a Haskell web server" )

-- | Entry point, post options parsing
entry :: Int -> Env -> IO ()
entry p env = do
  run p $ server' defApp
  where
    server' = gzip def . logStdoutDev . runMiddlewareT runAppT' server
    server  = securityLayer . staticLayer . contentLayer
    runAppT' = flip runAppT env


-- * Utilities

-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> Env
appOptsToEnv (AppOpts (Just p) (Just h) (Just c) (Just s)) =
  Env (UrlAuthority "http" True Nothing h $
        p <$ guard (p /= 80)
      ) c s
appOptsToEnv os = error $ "AppOpts improperly formatted: " ++ show os
