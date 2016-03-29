module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Effects exposing (..)
import StartApp

import Items.Views.Title as Title
import Items.Views.List  as List
import Items.Actions     as ItemsActions
import Model exposing(model, Model)

-- VIEW
view address model =
  div [ class "App" ]
    [ Title.view
    , List.view address model
    ]

-- INIT
app = StartApp.start
  { init   = (model, ItemsActions.getItems)
  , view   = view
  , update = update
  , inputs = [ ]
  }

main = app.html

update : ItemsActions.Action -> Model -> (Model, Effects ItemsActions.Action)
update = ItemsActions.update

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
