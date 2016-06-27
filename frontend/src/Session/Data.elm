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

import Session.Types exposing (..)

import Json.Encode as JsonE
import Json.Decode as JsonD exposing (Decoder, (:=))
import Cmd.Extra exposing (mkCmd)
import Task
import Dict exposing (Dict)
import Threading exposing (Threaded)


type alias ThreadId = Int

-- Ports

port makeSignature : Threaded String -> Cmd a

type alias MadeSignature =
  { publicKey : Hex
  , signature : Hex
  }

port madeSignature : (Threaded MadeSignature -> a) -> Sub a

port openSignature : Threaded Hex -> Cmd a

port openedSignature : (Threaded (Maybe String) -> a) -> Sub a


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

fromOpenedSignature : Threaded (Maybe String) -> Maybe String
fromOpenedSignature = .payload

-- Creation

type alias Model a =
  { signThreads : Threading.Model SignedRequest a
  , openThreads : Threading.Model (Maybe String) a
  }

type Msg a
  = Sign String         (SignedRequest -> Cmd a)
  | Open SignedResponse (Maybe String  -> Cmd a)
  | SignThreading (Threading.Msg String         SignedRequest  a)
  | OpenThreading (Threading.Msg SignedResponse (Maybe String) a)

init : (Model a, Cmd (Msg a))
init =
  ( { signThreads = Threading.init
    , openThreads = Threading.init
    }
  , Cmd.none
  )


update : Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    SignThreading a ->
      let (newSign, signEff) = Threading.update makeSignature a model.signThreads
      in  ( { model | signThreads = newSign }
          , Cmd.map (Result.formatError SignThreading) signEff
          )
    OpenThreading a ->
      let (newOpen, openEff) = Threading.update openSignature a model.openThreads
      in  ( { model | openThreads = newOpen }
          , Cmd.map (Result.formatError OpenThreading) openEff
          )
    Sign message onComplete ->
      ( model
      , mkCmd <| Err <| SignThreading <| Threading.Call message onComplete
      )
    Open signature onComplete ->
      ( model
      , mkCmd <| Err <| OpenThreading <| Threading.Call signature onComplete
      )


subscriptions : Sub (Msg a)
subscriptions =
  Sub.batch
    [ Sub.map SignThreading <|
        Threading.subscriptions <| madeSignature identity
    , Sub.map OpenThreading <|
        Threading.subscriptions <| openedSignature identity
    ]


