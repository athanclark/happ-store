module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Effects exposing (..)
import StartApp

import Root exposing (..)

import Items.Views.Title as Title
import Items.Views.List  as List
import Items.Actions     as ItemsActions
import Model exposing(model, Model)

-- INIT
app : StartApp.App (RootModel number)
app = StartApp.start
  { init   = (initRootModel 0, Effects.none)
  , view   = rootTemplate (\_ _ -> [text "yo"])
  , update = rootUpdate (\_ _ -> (0, Effects.none))
  , inputs = [ ]
  }

main : Signal.Signal Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
