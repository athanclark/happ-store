port module SHA exposing (..)


type alias Hashed = String


port askSHA : String -> Cmd a

port getSHA : (Hashed -> a) -> Sub a
