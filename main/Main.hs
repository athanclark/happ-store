{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Main where

import Main.Options (getEnv)
import Imports
import Schema
import Application
import Cabal

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import System.Remote.Monitoring as Monitor
import Data.TimeMap as TM
import Crypto.Saltine as NaCL

import Data.Url
import Data.Monoid
import Control.Monad
import Control.Concurrent.STM (atomically)
import Control.Concurrent



-- * Executable

main :: IO ()
main = do
  (env,p,m) <- getEnv
  putStrLn $ unlines
    [ "Cooperate  Copyright (C) 2016  Athan Clark"
    , "This program comes with ABSOLUTELY NO WARRANTY; for details see"
    , "Section 15 of the GNU Public License version 3, available in the LICENSE"
    , "file in the source distribution of this codebase, or at"
    , "<http://www.gnu.org/licenses/gpl-3.0.en.html>."
    , ""
    , "This is free software, and you are welcome to redistribute it"
    , "under certain conditions; see the GNU General Public License version 3"
    , "for details."
    , ""
    , "- port:       " <> show p
    , "- monitor:    " <> show m
    , "- hostname:   " <> showUrlAuthority (envAuthority env)
    , "- cwd:        " <> envCwd env
    , "- static:     " <> envStatic env
    , "- production: " <> show (envProduction env)
    ]
  entry p m env

-- | Entry point
entry :: Int -> Int -> Env -> IO ()
entry p m env = do
  when (envProduction env) NaCL.optimize

  -- monitor
  Monitor.forkServer "localhost" m

  let secondPico = 1000000
      secondDiff = 1
      hour = secondPico * 3600

  -- session cleaner
  forkIO . forever $ do
    let sessionCache = envSession env
    unless (envProduction env) $ do
      nonces <- atomically $ TM.toList sessionCache
      putStrLn $ "Current Cache: " ++ show nonces
    TM.filterFromNow (60 * secondDiff) sessionCache
    threadDelay (5 * secondPico)

  -- shallow fetcher
  forkIO . forever $ do
    updateFetchedShallow env
    threadDelay hour

  -- deep fetcher
  forkIO . forever $ do
    threadDelay $ 24 * hour
    updateFetchedDeep env

  -- main app
  runEnv p $ server' defApp
  where
    server'  = gzip def
            -- . logStdoutDev
             . runMiddlewareT runAppT' server
    server   = securityLayer
             . staticLayer
             . contentLayer
    runAppT' = flip runAppT env


