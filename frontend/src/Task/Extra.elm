module Task.Extra exposing (..)

import Task exposing (Task)


performLog : Task never a -> Cmd a
performLog = Task.perform (Debug.crash << toString) identity
