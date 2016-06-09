module Modals exposing (..)

import Html.App        as App
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Array exposing (Array)

type alias Fifo a = List a

pushFifo : a -> Fifo a -> Fifo a
pushFifo x = Array.toList << Array.push x << Array.fromList

peekFifo : Fifo a -> Maybe (a, Fifo a)
peekFifo xs =
  case xs of
    []       -> Nothing
    x :: xs' -> Just (x, xs')

type ModalType 

type ModalBuilder = ModalBuilder
  { 
  }


type alias Model =
  { visibility : Bool
  , opacity    : Float
  , tasks      : -- A dict of threads, plus their type..? Each type has a method
                 -- to opening, given type-specific config, and builds a continuation
                 -- for each modal implementation? One modal at a time anyway. Walk up
                 -- the ordered Dict then? Could just be a list... of... type-specific
                 -- configs * type itself?
  }

type Msg a
  = SessionDisconnect

init : (Model, Cmd (Msg a))
init =
  ( { visibility = False
    , opacity = 0
    }
  , Cmd.none
  )

update : Msg a -> Model -> (Model, Cmd (Msg a))
update action model =
  case action of
    SessionDisconnect ->
      if model.visibility
      then ( model
           , 
           )
