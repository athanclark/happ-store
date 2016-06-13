{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , MultiParamTypeClasses
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Application.Types where

import Cabal.Types
import Server.Types
import Schema

import Path.Extended
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.Monoid
import Data.Url
import Data.TimeMap as TM
import Data.Hashable
import Data.Data
import Data.Acid
import Data.Acid.Memory
import Data.STRef
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.ST
import Control.Concurrent.STM (atomically)
import Crypto.Saltine.Core.Sign as NaCl
import Crypto.Saltine.Class     as NaCl

import GHC.Generics


-- * Global Variables

-- ** Sessions

newtype ClientPublicKey = ClientPublicKey
  { getClientPublicKey :: BS.ByteString
  } deriving (Show, Eq, Hashable)

instance FromJSON ClientPublicKey where
  parseJSON (String s) =
    case BS16.decode $ T.encodeUtf8 s of
      (decoded, rest) | rest /= "" -> fail "Not base-16 encoded"
                      | otherwise  -> pure $ ClientPublicKey decoded
  parseJSON _ = fail "Not a string"

toNaClPublicKey :: ClientPublicKey -> Maybe NaCl.PublicKey
toNaClPublicKey (ClientPublicKey k) = NaCl.decode k


type SessionId = ClientPublicKey

-- A cache is a time-indexed mapping from nonces to accrued hashings
type SessionCache = TimeMap SessionId UserId


-- ** The Execution Environment

-- The environment accessible from our application
data Env = Env
  { envAuthority  :: UrlAuthority
  , envCwd        :: FilePath -- ^ for File Processing
  , envStatic     :: FilePath
  , envProduction :: Bool
  , envSession    :: SessionCache
  , envPublicKey  :: PublicKey
  , envSecretKey  :: SecretKey
  , envManager    :: Manager
  , envDatabase   :: AcidState Database
  , envFetched    :: STRef RealWorld Fetched
  }

instance Show Env where
  show (Env a c s p _ _ _ _ _ _) =
    "Env {envAuthority = " ++ show a ++ ", envCwd = "
                           ++ show c ++ ", envStatic = "
                           ++ show s ++ ", envProduction = "
                           ++ show p ++ ", envSession = <session>},\
                                         \ envPublicKey = <#>,\
                                         \ envSecretKey = <#>,\
                                         \ envManager = <manager>,\
                                         \ envDatabase = <database>,\
                                         \ envFetched = <fetched>}"

instance Eq Env where
  (Env a1 c1 s1 p1 _ _ _ _ _ _) == (Env a2 c2 s2 p2 _ _ _ _ _ _) =
    a1 == a2 && c1 == c2 && s1 == s2 && p1 == p2

-- | A really terrible environment value that should only be used with testing
emptyEnv :: IO Env
emptyEnv = do
  t       <- atomically TM.newTimeMap
  (sk,pk) <- NaCl.newKeypair
  m       <- newManager tlsManagerSettings
  db      <- openMemoryState initDB
  f       <- stToIO $ newSTRef emptyFetched
  let auth = UrlAuthority "http" True Nothing "localhost" Nothing
  pure Env { envAuthority  = auth
           , envCwd        = "/"
           , envStatic     = "/"
           , envProduction = False
           , envSession    = t
           , envPublicKey  = pk
           , envSecretKey  = sk
           , envManager    = m
           , envDatabase   = db
           , envFetched    = f
           }


-- * Application Effects Stack

type AppM a = LoggingT (ReaderT Env IO) a

runAppT :: AppM a -> Env -> IO a
runAppT = runReaderT . runStderrLoggingT

type AppTemplateT m = AbsoluteUrlT m

runAppTemplateT :: AppTemplateT m a -> UrlAuthority -> m a
runAppTemplateT = runAbsoluteUrlT


type MonadApp m =
  ( MonadIO m
  , MonadThrow m
  , MonadCatch m
  , MonadLogger m
  , MonadReader Env m
  , MonadUrl Abs File m
  , MonadBaseControl IO m
  , Typeable m
  )



-- * Inter-App Hrefs

-- | Data type representing top navigation bar
data AppLinks
  = AppHome
  | AppAbout
  | AppContact
  deriving (Show, Eq)

instance ToPath AppLinks Abs File where
  toPath AppHome    = parseAbsFile "/index"
  toPath AppAbout   = parseAbsFile "/about"
  toPath AppContact = parseAbsFile "/contact"

instance ToLocation AppLinks Abs File where
  toLocation x = fromPath <$> toPath x


data AppResources
  = JQuery
  | JQueryCdn
  | SemanticJs
  | SemanticJsCdn
  | SemanticCss
  | SemanticCssCdn
  | JsSHA
  | JsSHACdn
  | JsNaCl
  | JsNaClCdn
  | LessStyles
  | AppFrontend
  deriving (Show, Eq)

instance ToPath AppResources Abs File where
  toPath JQuery           = parseAbsFile "/vendor/jquery/dist/jquery"
  toPath SemanticJs       = parseAbsFile "/vendor/semantic-ui/dist/semantic"
  toPath SemanticCss      = parseAbsFile "/vendor/semantic-ui/dist/semantic"
  toPath JQueryCdn        = parseAbsFile "/ajax/libs/jquery/3.0.0-beta1/jquery"
  toPath SemanticJsCdn    = parseAbsFile "/ajax/libs/semantic-ui/2.1.8/semantic"
  toPath SemanticCssCdn   = parseAbsFile "/ajax/libs/semantic-ui/2.1.8/semantic"
  toPath JsSHACdn         = parseAbsFile "/ajax/libs/jsSHA/2.1.0/sha"
  toPath JsSHA            = parseAbsFile "/vendor/jsSHA/src/sha"
  toPath JsNaCl           = parseAbsFile "/vendor/js-nacl/lib/nacl_factory"
  toPath JsNaClCdn        = parseAbsFile "/ajax/libs/js-nacl/1.2.0/nacl_factory"
  toPath LessStyles       = parseAbsFile "/style"
  toPath AppFrontend      = parseAbsFile "/Main"

instance ToLocation AppResources Abs File where
  toLocation JQuery           = (addFileExt "min.js"  . fromPath) <$> toPath JQuery
  toLocation SemanticJs       = (addFileExt "min.js"  . fromPath) <$> toPath SemanticJs
  toLocation SemanticCss      = (addFileExt "min.css" . fromPath) <$> toPath SemanticCss
  toLocation JQueryCdn        = (addFileExt "min.js"  . fromPath) <$> toPath JQueryCdn
  toLocation SemanticJsCdn    = (addFileExt "min.js"  . fromPath) <$> toPath SemanticJsCdn
  toLocation SemanticCssCdn   = (addFileExt "min.css" . fromPath) <$> toPath SemanticCssCdn
  toLocation JsSHACdn         = (addFileExt "js"      . fromPath) <$> toPath JsSHACdn
  toLocation JsSHA            = (addFileExt "js"      . fromPath) <$> toPath JsSHA
  toLocation JsNaCl           = (addFileExt "js"      . fromPath) <$> toPath JsNaCl
  toLocation JsNaClCdn        = (addFileExt "min.js"  . fromPath) <$> toPath JsNaClCdn
  toLocation LessStyles       = (addFileExt "css"     . fromPath) <$> toPath LessStyles
  toLocation AppFrontend      = (addFileExt "min.js"  . fromPath) <$> toPath AppFrontend

appendActiveWhen :: AppLinks -> Maybe AppLinks -> T.Text -> T.Text
appendActiveWhen x (Just y) c | x == y = c <> " active"
appendActiveWhen _ _ c = c


-- * Exceptions

data SessionException
  = InvalidSignedRequest String
  | BadSessionFormat
  | NonexistentSessionId SessionId
  deriving (Generic, Show)

instance Exception SessionException

data LoginException
  = BadLoginFormat
  deriving (Generic, Show)

instance Exception LoginException
