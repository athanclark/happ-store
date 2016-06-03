port module Session.Nonce exposing
  ( Nonce
  , Model
  , Msg (NewNonce)
  , init
  , update
  , subscriptions
  )

{-| This is a singleton module -}

import Cmd.Extra exposing (mkCmd)
import Uuid
import Random.Pcg exposing (step, Seed, initialSeed2)
import Dict exposing (Dict)


port askInitNonce : ThreadId -> Cmd a

type alias GetInitNonce =
  { seed1    : Int
  , seed2    : Int
  , threadId : ThreadId
  }

port getInitNonce : (GetInitNonce -> a) -> Sub a

type alias ThreadId = Int
type alias Nonce = Uuid.Uuid

type alias Model a =
  { threads      : Dict ThreadId (Nonce -> Cmd a)
  , threadId     : ThreadId
  }

freshThreadId : Model a -> (ThreadId, Model a)
freshThreadId model =
  ( model.threadId
  , { model | threadId = model.threadId + 1 }
  )

type Msg a
  = NewNonce (Nonce -> Cmd a)
  | GotFresh GetInitNonce

init : (Model a, Cmd (Msg a))
init =
  ( { threads  = Dict.empty
    , threadId = 0
    }
  , Cmd.none
  )

update : Msg a -> Model a -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    NewNonce onComplete ->
      let (threadId, model') = freshThreadId model
      in  ( { model' | threads = Dict.insert
                                   threadId
                                   onComplete
                                   model'.threads
            }
          , askInitNonce threadId
          )
    GotFresh xs ->
      case Dict.get xs.threadId model.threads of
        Nothing -> ( model
                   , Cmd.none
                   )
        Just onComplete ->
          let (newNonce, _) = step Uuid.uuidGenerator
                                <| initialSeed2 xs.seed1 xs.seed2
          in  ( { model | threads = Dict.remove xs.threadId model.threads }
              , Cmd.map Ok <| onComplete newNonce
              )

subscriptions : Sub (Msg a)
subscriptions = getInitNonce GotFresh

