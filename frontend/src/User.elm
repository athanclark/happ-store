module User exposing (..)

import SHA


type alias PlainCredentials =
  { username : String
  , password : SHA.Hashed
  }

type Credentials
  = Plain PlainCredentials

