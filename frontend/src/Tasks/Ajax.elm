module Tasks.Ajax where

import Effects exposing (..)
import Http
import Task
import Json.Decode as Json

get : String
   -> Json.Decoder a
   -> (Maybe a -> action)
   -> Effects action
get url decoder action =
  Http.get decoder url
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task
