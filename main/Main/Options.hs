{-# LANGUAGE
    DeriveGeneric
  #-}

module Main.Options where

import Cabal.Types
import Application.Types
import Schema

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import Data.Default
import System.Directory
import Path.Extended
import Crypto.Saltine.Core.Sign as NaCl
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.TimeMap as TM
import Data.Url
import Data.Monoid
import Data.Acid
import Data.STRef

import GHC.Generics
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Control.Concurrent.STM (atomically)
import Control.Concurrent.QSem
import Control.Exception (bracket)


-- * Options Parsing

-- | Application-wide options
data AppOpts = AppOpts
  { port       :: Maybe Int
  , monitor    :: Maybe Int
  , host       :: Maybe String
  , cwd        :: Maybe FilePath
  , static     :: Maybe FilePath
  , production :: Maybe Bool
  , verbose    :: Maybe Bool
  , limitFetch :: Maybe Int
  , limitHtml  :: Maybe Int
  , limitJson  :: Maybe Int
  } deriving (Show, Eq, Generic)

instance Monoid AppOpts where
  mempty = AppOpts { port       = Nothing
                   , monitor    = Nothing
                   , host       = Nothing
                   , cwd        = Nothing
                   , static     = Nothing
                   , production = Nothing
                   , verbose    = Nothing
                   , limitFetch = Nothing
                   , limitHtml  = Nothing
                   , limitJson  = Nothing
                   }
  mappend (AppOpts p1 m1 h1 c1 s1 pr1 v1 lf1 lh1 lj1)
          (AppOpts p2 m2 h2 c2 s2 pr2 v2 lf2 lh2 lj2) =
    AppOpts { port       = getLast $ Last p1 <> Last p2
            , monitor    = getLast $ Last m1 <> Last m2
            , host       = getLast $ Last h1 <> Last h2
            , cwd        = getLast $ Last c1 <> Last c2
            , static     = getLast $ Last s1 <> Last s2
            , production = getAny <$> (Any <$> pr1) <> (Any <$> pr2)
            , verbose    = getAny <$> (Any <$> v1)  <> (Any <$> v2)
            , limitFetch = getLast $ Last lf1 <> Last lf2
            , limitHtml  = getLast $ Last lh1 <> Last lh2
            , limitJson  = getLast $ Last lj1 <> Last lj2
            }

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

instance Default AppOpts where
  def = AppOpts { port       = Just 3000
                , monitor    = Just 3001
                , host       = Just "localhost"
                , cwd        = Nothing
                , static     = Nothing
                , production = Just False
                , verbose    = Just False
                , limitFetch = Just 100
                , limitHtml  = Just 100
                , limitJson  = Just 1000
                }

appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> optional ( option auto
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on - DEF: 3000" ))
  <*> optional ( option auto
        ( long "monitor"
       <> short 'm'
       <> metavar "MONITOR"
       <> help "port for process monitor - DEF: 3001" ))
  <*> optional ( strOption
        ( long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "host to deploy URLs over - DEF: 'localhost'" ))
  <*> optional ( strOption
        ( long "cwd"
       <> short 'c'
       <> metavar "CWD"
       <> help "absolute directory to search for configuration files - DEF: `pwd`/" ))
  <*> optional ( strOption
        ( long "static"
       <> short 's'
       <> metavar "STATIC"
       <> help "absolute directory to search for servable static files\
               \ - DEF: `pwd`/static/" ))
  <*> optional ( switch
        ( long "production"
       <> help "run the app in production mode" ))
  <*> optional ( switch
        ( long "verbose"
       <> short 'v'
       <> help "log details; http requests, cache fetches" ))
  <*> optional ( option auto
        ( long "limit-fetch"
       <> help "max number of concurrent hackage data minig requests - DEF: 100"
       <> metavar "LIMFETCH" ))
  <*> optional ( option auto
        ( long "limit-html"
       <> metavar "LIMHTML"
       <> help "max number of active concurrent html requests - DEF: 100" ))
  <*> optional ( option auto
        ( long "limit-json"
       <> metavar "LIMJSON"
       <> help "max number of active concurrent json requests - DEF: 1000" ))

-- | Command-line options
data App = App
  { options    :: AppOpts
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


getEnv :: IO (AcidState Database -> Env, Int, Int, Bool, Int, Int, Int)
getEnv = do
  let opts :: ParserInfo App
      opts = info (helper <*> app)
        ( fullDesc
       <> progDesc "Serve application over PORT as HOST under CWD and STATIC, with MONITOR"
       <> header "hApp-Store - an app store for Hackage" )
  commandOpts <- execParser opts :: IO App
  cwd'        <- getCurrentDirectory
  let yamlConfigPath' =
        fromMaybe
          (cwd' <> "/config/app.yaml")
          (configPath commandOpts)

  yamlConfigPath     <- toFilePath <$> parseAbsFile yamlConfigPath'
  yamlConfigExists   <- doesFileExist yamlConfigPath
  yamlConfigContents <- if yamlConfigExists
                        then readFile yamlConfigPath
                        else return ""
  mYamlConfig <- if yamlConfigExists && yamlConfigContents /= ""
                 then Y.decodeFile yamlConfigPath
                 else return Nothing

  let yamlConfig = fromMaybe def mYamlConfig
      dirs       = AppOpts
                     Nothing
                     Nothing
                     Nothing
                     (Just $ cwd' <> "/")
                     (Just $ cwd' <> "/static/")
                     Nothing
                     Nothing
                     Nothing
                     Nothing
                     Nothing
      config = dirs <> def <> yamlConfig <> options commandOpts

  cwdDir       <- toFilePath <$> parseAbsDir (fromJust $ cwd config)
  staticDir    <- toFilePath <$> parseAbsDir (fromJust $ static config)

  cwdExists    <- doesDirectoryExist cwdDir
  staticExists <- doesDirectoryExist staticDir

  case (cwdExists,staticExists) of
    (False,_) -> error $ "--cwd argument `"    <> cwdDir    <> "` does not exist"
    (_,False) -> error $ "--static argument `" <> staticDir <> "` does not exist"
    _         -> do env <- appOptsToEnv config
                    pure ( env
                         , fromJust $ port config
                         , fromJust $ monitor config
                         , fromJust $ production config
                         , fromJust $ limitFetch config
                         , fromJust $ limitHtml config
                         , fromJust $ limitJson config
                         )




-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> IO (AcidState Database -> Env)
appOptsToEnv (AppOpts (Just p)
                      _
                      (Just h)
                      (Just c)
                      (Just s)
                      (Just pr)
                      (Just v)
                      (Just lf)
                      (Just lh)
                      (Just lj)) = do
  t       <- atomically TM.newTimeMap
  (sk,pk) <- NaCl.newKeypair
  m       <- newManager tlsManagerSettings
  f       <- stToIO $ newSTRef emptyFetched
  lf'     <- newQSem lf
  lh'     <- newQSem lh
  lj'     <- newQSem lj
  let auth = UrlAuthority "http" True Nothing h $ p <$ guard (p /= 80)
  pure $ \db -> Env
         { envAuthority  = auth
         , envCwd        = c
         , envStatic     = s
         , envProduction = pr
         , envVerbose    = v
         , envSession    = t
         , envPublicKey  = pk
         , envSecretKey  = sk
         , envManager    = m
         , envDatabase   = db
         , envFetched    = f
         , envQueues     = Queues lf' lh' lj'
         }
appOptsToEnv os = error $ "AppOpts improperly formatted: " ++ show os
