port module Session.Nonce exposing
  ( Nonce
  , Model
  , Msg (NewNonce)
  , init
  , update
  , subscriptions
  )

import Cmd.Extra exposing (mkCmd)
import Uuid
import Random.Pcg exposing (step, Seed, initialSeed2)



type alias Nonce = Uuid.Uuid

type alias Model =
  { currentSeed  : Seed
  , currentNonce : Maybe Nonce
  }

type Msg
  = NewNonce
  | NewNonce'
  | SetFresh (Int, Int)

init : (Model, Cmd Msg)
init =
  ( { currentSeed  = initialSeed2 0 0 -- uncurry initialSeed2 fs
    , currentNonce = Nothing
    }
  , Cmd.none
  )

update : (Nonce -> Cmd a) -> Msg -> Model -> (Model, Cmd (Result Msg a))
update fulfil action model =
  case action of
    NewNonce ->
      ( model
      , askInitNonce ()
      )
    NewNonce' ->
      let (newNonce, newSeed) = step Uuid.uuidGenerator model.currentSeed
      in  ( { model | currentSeed  = newSeed
                    , currentNonce = Just newNonce
            }
          , Cmd.map Ok <| fulfil newNonce
          )
    SetFresh xs ->
      ( { model | currentSeed = uncurry initialSeed2 xs }
      , mkCmd <| Err NewNonce'
      )

subscriptions : Sub Msg
subscriptions = getInitNonce SetFresh


port askInitNonce : () -> Cmd a

port getInitNonce : ((Int, Int) -> a) -> Sub a
