{-# LANGUAGE
    DeriveDataTypeable
  #-}

module Schema where

import Imports

import Data.Data
import Data.Typeable


data Person = Person
  { personName     :: String
  , personPersonas :: [Persona]
  } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | A href link to a source of the person's work, for instance a twitter
--   handle, a blog link, a personal website, a github page, etc.
type Persona = String


