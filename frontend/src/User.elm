module User exposing (..)

import Json.Encode as JsonE


type alias PlainCredentials =
  { username : String
  , password : String
  }


type Credentials
  = Plain PlainCredentials

encodeCredentials : Credentials -> JsonE.Value
encodeCredentials (Plain cs) =
  JsonE.object
    [ ( "username"
      , JsonE.string cs.username
      )
    , ( "password"
      , JsonE.string cs.password
      )
    ]
