module Cmd.Extra exposing (..)

import Task


mkCmd : a -> Cmd a
mkCmd x =
  Task.perform Debug.crash identity
    <| Task.succeed x
