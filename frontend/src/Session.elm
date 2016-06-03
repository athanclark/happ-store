module Session exposing
  ( Model
  , Msg (Login)
  , init
  , update
  , subscriptions
  , viewMenuItem
  )

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Session.Nonce as Nonce
import Session.Data  as Data
import User
import Uuid
import Http
import Json.Decode as JsonD
import Json.Encode as JsonE
import Task
import Task.Extra as Task
import Cmd.Extra exposing (mkCmd)


type alias Session =
  { user  : ()
  , nonce : Nonce.Nonce
  }


type alias Model =
  { session        : Maybe Session
  , nonceState     : Nonce.Model
  , dataState      : Data.Model Msg
  , tmpCredentials : Maybe User.Credentials
  }

type Msg
  = Login User.Credentials
  | NonceMsg Nonce.Msg
  | DataMsg  (Data.Msg Msg)
  | GotNonce Uuid.Uuid
  | MadeSession Data.WithSession
  | CheckedSession Bool
  | LoginResponse String

init : (Model, Cmd Msg)
init =
  let (nonceModel, nonceEff) = Nonce.init
      (dataModel,  dataEff)  = Data.init
  in  ( { session        = Nothing
        , nonceState     = nonceModel
        , dataState      = dataModel
        , tmpCredentials = Nothing
        }
      , Cmd.batch
          [ Cmd.map NonceMsg nonceEff
          , Cmd.map DataMsg dataEff
          ]
      )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Login cs ->
      ( { model | tmpCredentials = Just cs }
      , mkCmd <| NonceMsg Nonce.NewNonce
      )
    GotNonce u ->
      ( model
      , case model.tmpCredentials of
          Nothing -> Debug.crash "somehow lock failed"
          Just cs -> Task.performLog
          <| Http.post
               (JsonD.map LoginResponse JsonD.string)
               "/login"
               (Http.string <| Uuid.toString u)
      )
    LoginResponse s -> Debug.log s
      ( model
      , Cmd.none
      )
    NonceMsg a ->
      let (newNonce, eff) = Nonce.update
                              (mkCmd << GotNonce)
                              a
                              model.nonceState
      in  ( { model | nonceState = newNonce }
          , Cmd.map (\r -> case r of
                             Err x -> NonceMsg x
                             Ok  x -> x) eff
          )
    DataMsg a ->
      let (newData, eff) = Data.update
                             a
                             model.dataState
      in  ( { model | dataState = newData }
          , Cmd.map (\r -> case r of
                             Err x -> DataMsg x
                             Ok  x -> x) eff
          )
    MadeSession s ->
      ( model
      , Cmd.none
      )
    CheckedSession s ->
      ( model
      , Cmd.none
      )

subscriptions : Sub Msg
subscriptions =
  Sub.batch
    [ Sub.map NonceMsg Nonce.subscriptions
    ]


viewMenuItem : Model -> Html Msg
viewMenuItem model =
  a [ class "item"
    , onClick <| Login <| User.Plain { username = "", password = "" }
    ]
    [ case model.session of
        Nothing -> text "login"
        Just u  -> text "sup dawg!"
    ]
