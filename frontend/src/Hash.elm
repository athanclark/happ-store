module Hash exposing
  ( Msg (GetHash)
  , Hashed
  , update
  , subscriptions
  )

import SHA

type alias Hashed = SHA.Hashed

type Msg
  = GetHash String
  | GotHash Hashed

update : (Hashed -> Cmd a) -> Msg -> Cmd (Result a Msg)
update fulfil action =
  case action of
    GetHash s ->
      SHA.askSHA s
    GotHash h ->
      Cmd.map Err <| fulfil h

subscriptions : Sub Msg
subscriptions =
  SHA.getSHA GotHash
