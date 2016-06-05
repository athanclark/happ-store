port module Session.Data exposing
  ( WithSession
  , Hashed
  , encodeWithSession
  , decodeWithSession
  , Model
  , Msg (MkWithSession, CkWithSession)
  , init
  , update
  , subscriptions
  )

{-|
Note that this module should be used as a singleton
-}

import Session.Nonce as Nonce
import Uuid
import Json.Encode as JsonE
import Json.Decode as JsonD exposing (Decoder, (:=))
import Cmd.Extra exposing (mkCmd)
import Task
import Dict exposing (Dict)


type alias ThreadId = Int
type alias Hashed = String


type alias MakeSHASession =
  { threadId : ThreadId
  , input    : String
  }

port makeSHASession : MakeSHASession -> Cmd a

type alias MadeSHASession =
  { threadId : ThreadId
  , output   : Hashed
  }

port madeSHASession : (MadeSHASession -> a) -> Sub a



type alias WithSession =
  { nonce : Nonce.Nonce
  , data  : JsonE.Value
  , hash  : Hashed
  }

encodeWithSession : WithSession -> JsonE.Value
encodeWithSession { nonce, data, hash } =
  JsonE.object
    [ ( "nonce"
      , JsonE.string <| Uuid.toString nonce
      )
    , ( "data", data
      )
    , ( "hash", JsonE.string hash
      )
    ]

decodeWithSession : Decoder WithSession
decodeWithSession =
  JsonD.object3 WithSession
    ("nonce" := JsonD.string `JsonD.andThen` \s ->
                  case Uuid.fromString s of
                    Nothing -> JsonD.fail "improperly formatted UUID"
                    Just x  -> JsonD.succeed x)
    ("data" := JsonD.value)
    ("hash" := JsonD.string)

-- Creation

type alias Model a =
  { threadId  : ThreadId
  , mkThreads : Dict ThreadId (Nonce.Nonce, JsonE.Value, WithSession -> Cmd a)
  , ckThreads : Dict ThreadId (WithSession, Bool -> Cmd a)
  }

freshThreadId : Model a -> (ThreadId, Model a)
freshThreadId model =
  ( model.threadId
  , { model | threadId = model.threadId + 1 }
  )

type Msg a
  = MkWithSession Nonce.Nonce JsonE.Value (WithSession -> Cmd a)
  | CkWithSession Hashed WithSession (Bool -> Cmd a)
  | GotHash MadeSHASession

init : (Model a, Cmd (Msg a))
init =
  ( { threadId  = 0
    , mkThreads = Dict.empty
    , ckThreads = Dict.empty
    }
  , Cmd.none
  )


update : Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    MkWithSession nonce data onComplete ->
      let nonce' = Uuid.toString nonce
          data'  = JsonE.encode 0 data
          (threadId, model') = freshThreadId model
      in  ( { model' | mkThreads = Dict.insert
                                     threadId
                                     (nonce, data, onComplete)
                                     model.mkThreads
            }
          , makeSHASession
              { threadId = threadId
              , input    = nonce' ++ data'
              }
          )
    CkWithSession lastHash session onComplete ->
      let nonce' = Uuid.toString session.nonce
          data'  = JsonE.encode 0 session.data
          (threadId, model') = freshThreadId model
      in  ( { model' | ckThreads = Dict.insert
                                     threadId
                                     (session, onComplete)
                                     model.ckThreads
            }
          , makeSHASession
              { threadId = threadId
              , input    = nonce' ++ data' ++ lastHash
              }
          )
    GotHash hashed ->
      case Dict.get hashed.threadId model.mkThreads of
        Nothing ->
          case Dict.get hashed.threadId model.ckThreads of
            Nothing ->
              ( model
              , Cmd.none
              )
            Just (session, onComplete) ->
              ( { model | ckThreads = Dict.remove
                                        hashed.threadId
                                        model.ckThreads
                }
              , Cmd.map Ok <| onComplete <| hashed.output == session.hash
              )
        Just (nonce, data, onComplete) ->
          ( { model | mkThreads = Dict.remove
                                    hashed.threadId
                                    model.mkThreads
            }
          , Cmd.map Ok <| onComplete
              { nonce = nonce
              , data  = data
              , hash  = hashed.output
              }
          )


subscriptions : Sub (Msg a)
subscriptions =
  madeSHASession GotHash


