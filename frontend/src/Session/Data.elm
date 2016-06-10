port module Session.Data exposing
  ( SignedRequest
  , SignedResponse
  , encodeSignedRequest
  , signedResponseDecoder
  , Model
  , Msg (Sign, Open)
  , init
  , update
  , subscriptions
  )

{-|
Note that this module should be used as a singleton
-}

import Json.Encode as JsonE
import Json.Decode as JsonD exposing (Decoder, (:=))
import Cmd.Extra exposing (mkCmd)
import Task
import Dict exposing (Dict)


type alias ThreadId = Int
type alias Hex = String

-- Ports

type alias MakeSignature =
  { threadId : ThreadId
  , toSign   : String
  }

port makeSignature : MakeSignature -> Cmd a

type alias MadeSignature =
  { threadId  : ThreadId
  , publicKey : Hex
  , signature : Hex
  }

port madeSignature : (MadeSignature -> a) -> Sub a


type alias OpenSignature =
  { threadId : ThreadId
  , toVerify : Hex
  }

port openSignature : OpenSignature -> Cmd a

type alias OpenedSignature =
  { threadId : ThreadId
  , verified : Maybe String
  }

port openedSignature : (OpenedSignature -> a) -> Sub a


-- Application

type alias SignedRequest =
  { publicKey : Hex
  , signature : Hex
  }

encodeSignedRequest : SignedRequest -> JsonE.Value
encodeSignedRequest { publicKey, signature } =
  JsonE.object
    [ ("publicKey", JsonE.string publicKey)
    , ("signature", JsonE.string signature)
    ]

fromMadeSignature : MadeSignature -> SignedRequest
fromMadeSignature { publicKey, signature } =
  { publicKey = publicKey, signature = signature }


type alias SignedResponse = Hex

signedResponseDecoder : Decoder SignedResponse
signedResponseDecoder = JsonD.string

fromOpenedSignature : OpenedSignature -> Maybe String
fromOpenedSignature { verified } = verified

-- Creation

type alias Model a =
  { threadId  : ThreadId
  , signThreads : Dict ThreadId (SignedRequest -> Cmd a)
  , openThreads : Dict ThreadId (Maybe String  -> Cmd a)
  }

freshThreadId : Model a -> (ThreadId, Model a)
freshThreadId model =
  ( model.threadId
  , { model | threadId = model.threadId + 1 }
  )

type Msg a
  = Sign String         (SignedRequest -> Cmd a)
  | Open SignedResponse (Maybe String  -> Cmd a)
  | Signed MadeSignature
  | Opened OpenedSignature

init : (Model a, Cmd (Msg a))
init =
  ( { threadId  = 0
    , signThreads = Dict.empty
    , openThreads = Dict.empty
    }
  , Cmd.none
  )


update : Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    Sign message onComplete ->
      let (threadId, model') = freshThreadId model
      in  ( { model' | signThreads = Dict.insert
                                       threadId
                                       onComplete
                                       model.signThreads
            }
          , makeSignature
              { threadId = threadId
              , toSign   = message
              }
          )
    Open signature onComplete ->
      let (threadId, model') = freshThreadId model
      in  ( { model' | openThreads = Dict.insert
                                       threadId
                                       onComplete
                                       model.openThreads
            }
          , openSignature
              { threadId = threadId
              , toVerify = signature
              }
          )
    Signed made ->
      case Dict.get made.threadId model.signThreads of
        Nothing -> (model, Cmd.none)
        Just onComplete ->
          ( { model | signThreads = Dict.remove
                                      made.threadId
                                      model.signThreads
            }
          , Cmd.map Ok <| onComplete <| fromMadeSignature made
          )
    Opened opened ->
      case Dict.get opened.threadId model.openThreads of
        Nothing ->
          (model, Cmd.none)
        Just onComplete ->
          ( { model | openThreads = Dict.remove
                                      opened.threadId
                                      model.openThreads
            }
          , Cmd.map Ok <| onComplete <| fromOpenedSignature opened
          )


subscriptions : Sub (Msg a)
subscriptions =
  Sub.batch
    [ madeSignature Signed
    , openedSignature Opened
    ]


