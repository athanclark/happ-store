port module SHA exposing (..)

port askSHA : String -> Cmd a

port getSHA : (String -> a) -> Sub a
