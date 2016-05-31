module Session.Data exposing
  ( WithSession
  , encodeWithSession
  , Model
  , Msg (MkWithSession, CkWithSession)
  , init
  , update
  , subscriptions
  )

import Session.Nonce as Nonce
import Hash
import Uuid
import Json.Encode as JsonE
import Json.Decode as JsonD
import Cmd.Extra exposing (mkCmd)
import Task


type alias WithSession =
  { nonce : Nonce.Nonce
  , data  : JsonE.Value
  , hash  : Hash.Hashed
  }

encodeWithSession : WithSession -> JsonE.Value
encodeWithSession { nonce, data, hash } =
  JsonE.object
    [ ( "nonce"
      , JsonE.string <| Uuid.toString nonce
      )
    , ( "data"
      , data
      )
    , ( "hash"
      , JsonE.string hash
      )
    ]

-- Creation

type HashFor
  = Creation
  | Validation

type alias Model =
  { hashFor    : HashFor
  , tmpNonce   : Maybe Nonce.Nonce
  , tmpData    : Maybe JsonE.Value
  , tmpSession : Maybe WithSession
  }

type Msg
  = MkHashMsg Hash.Msg
  | CkHashMsg Hash.Msg
  | MkGotHash Hash.Hashed
  | CkGotHash Hash.Hashed
  | MkWithSession Nonce.Nonce JsonE.Value
  | CkWithSession WithSession

init : (Model, Cmd Msg)
init =
  ( { hashFor    = Creation
    , tmpNonce   = Nothing
    , tmpData    = Nothing
    , tmpSession = Nothing
    }
  , Cmd.none
  )

type CkError
  = FoulHash

update : (WithSession -> Cmd a)
      -> (Result CkError (Nonce.Nonce, JsonE.Value) -> Cmd a)
      -> Msg
      -> Model
      -> (Model, Cmd (Result Msg a))
update fulfilMk fulfilCk action model =
  case action of
    MkHashMsg a ->
      ( model
      , Cmd.map (\r -> case r of
                         Err a -> Err a
                         Ok  x -> Err <| MkHashMsg x)
          <| Hash.update (mkCmd << MkGotHash) a
      )
    CkHashMsg a ->
      ( model
      , Cmd.map (\r -> case r of
                         Err a -> Err a
                         Ok  x -> Err <| MkHashMsg x)
          <| Hash.update (mkCmd << CkGotHash) a
      )
    MkWithSession nonce data ->
      let nonce' = Uuid.toString nonce
          data'  = JsonE.encode 0 data
      in  ( { model | hashFor  = Creation
                    , tmpNonce = Just nonce
                    , tmpData  = Just data
            }
          , Task.perform Debug.crash (Err << MkHashMsg)
              <| Task.succeed <| Hash.GetHash
              <| nonce' ++ data'
          )
    CkWithSession session ->
      let nonce' = Uuid.toString session.nonce
          data'  = JsonE.encode 0 session.data
      in  ( { model | hashFor    = Validation
                    , tmpSession = Just session
            }
          , Task.perform Debug.crash (Err << CkHashMsg)
              <| Task.succeed <| Hash.GetHash
              <| nonce' ++ data'
          )
    MkGotHash hash ->
      case (model.tmpNonce, model.tmpData) of
        (Just nonce, Just data) ->
          let session = { nonce = nonce
                        , data  = data
                        , hash  = hash
                        }
          in ( { model | tmpNonce = Nothing
                       , tmpData  = Nothing
               }
             , Cmd.map Ok <| fulfilMk session
             )
        _ -> Debug.crash "no temp data"
    CkGotHash hash ->
      case model.tmpSession of
        Just session ->
          let result = if session.hash == hash
                       then Ok (session.nonce, session.data)
                       else Err FoulHash
          in ( { model | tmpSession = Nothing
               }
             , Cmd.map Ok <| fulfilCk result
             )
        _ -> Debug.crash "no temp data"


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.hashFor of
    Creation   -> Sub.map MkHashMsg Hash.subscriptions
    Validation -> Sub.map CkHashMsg Hash.subscriptions


