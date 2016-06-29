port module Session.Keys exposing
  ( Keypair
  , Msg (MakeKeypair)
  , Model
  , init
  , update
  , subscriptions
  )

import Session.Types exposing (..)

import Threading exposing (Threaded)
import Cmd.Extra exposing (mkCmd)


port makeKeypair : Threaded () -> Cmd a

type alias Keypair =
  { publicKey : Hex
  , secretKey : Hex
  }

port madeKeypair : (Threaded Keypair -> a) -> Sub a


type Msg a
  = ThreadingMsg (Threading.Msg () Keypair a)
  | MakeKeypair (Keypair -> Cmd a)

type alias Model a =
  { threads : Threading.Model Keypair a
  }

init : (Model a, Cmd (Msg a))
init =
  ( { threads = Threading.init }
  , Cmd.none
  )

update : Msg a -> Model a -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    ThreadingMsg a ->
      let (newThread, eff) = Threading.update makeKeypair a model.threads
      in  ( { threads = newThread }
          , Cmd.map (Result.formatError ThreadingMsg) eff
          )
    MakeKeypair onComplete ->
      ( model
      , mkCmd <| Err <| ThreadingMsg <| Threading.Call () onComplete
      )

subscriptions : Model a -> Sub (Msg a)
subscriptions model = Sub.map ThreadingMsg
                   <| Threading.subscriptions
                   <| madeKeypair identity
