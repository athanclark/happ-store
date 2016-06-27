port module Session.Keys exposing
  (
  )

import Session.Types exposing (..)

import Threading exposing (Treaded)


port makeKeypair : () -> Cmd a

type alias Keypair =
  { publicKey : Hex
  , secretKey : Hex
  }

port madeKeypair : (Keypair -> a) -> Sub a


