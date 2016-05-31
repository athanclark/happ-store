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
import User exposing (Credentials)
import Uuid
import Http
import Json.Decode as JsonD
import Task
import Cmd.Extra exposing (mkCmd)


type alias Session =
  { user  : ()
  , nonce : Nonce.Nonce
  }


type alias Model =
  { session    : Maybe Session
  , nonceState : Nonce.Model
  }

type Msg
  = Login Credentials
  | NonceMsg Nonce.Msg
  | GotNonce Uuid.Uuid
  | LoginResponse String

init : (Model, Cmd Msg)
init =
  let (nonceModel, nonceEff) = Nonce.init
  in  ( { session    = Nothing
        , nonceState = nonceModel
        }
      , Cmd.batch
          [ Cmd.map NonceMsg nonceEff
          ]
      )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Login cs ->
      ( model
      , mkCmd <| NonceMsg Nonce.NewNonce
      )
    GotNonce u ->
      ( model
      , Task.perform (Debug.crash << toString) identity
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
