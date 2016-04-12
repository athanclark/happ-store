module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Effects exposing (..)
import StartApp

import Root    exposing (..)
import Content exposing (..)

import Items.Views.Title as Title
import Items.Views.List  as List
import Items.Actions     as ItemsActions
import Model exposing(model, Model)

-- INIT
app : StartApp.App (RootModel ContentModel)
app = StartApp.start
  { init   = (initRootModel initContentModel, Effects.none)
  , view   = rootTemplate contentView
  , update = rootUpdate contentUpdate
  , inputs = [ ]
  }

main : Signal.Signal Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
