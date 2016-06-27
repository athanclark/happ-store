module Modals.Login exposing
  ( Model
  , Msg (Open, Close)
  , init
  , update
  , view
  , subscriptions
  )

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Duration
import Ease
import Time exposing (Time, millisecond)
import Cmd.Extra exposing (mkCmd)


type alias Model a =
  { visibility : Float
  , timeLeft   : Int
  , onRetry    : Cmd a
  , onLogout   : Cmd a
  , duration   : Duration.Model a
  }

type alias OpenParams a =
  { onSubmit : Cmd a
  }
