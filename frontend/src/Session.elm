module Session exposing
  ( Model
  , Msg (Login)
  , init
  , update
  , subscriptions
  , viewMenuItem
  )

import Session.Nonce as Nonce
import Session.Data  as Data
import User

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Every
import Http
import Json.Decode as JsonD exposing (Decoder, (:=))
import Json.Encode as JsonE
import Uuid
import Task
import Task.Extra as Task
import Time exposing (Time)
import Cmd.Extra exposing (mkCmd)


type alias Session =
  { user  : ()
  , nonce : Nonce.Nonce
  , hash  : String
  }

loginDecoder : Decoder Session
loginDecoder =
  Data.decodeWithSession `JsonD.andThen` \ws ->
    JsonD.succeed { user = ()
                  , nonce = ws.nonce
                  , hash  = ws.hash
                  }

fromSession : Session -> Data.WithSession
fromSession s =
  { nonce = s.nonce
  , hash  = s.hash
  , data  = JsonE.string "login success!"
  }

toSession : Data.WithSession -> Maybe Session
toSession s = Just
  { nonce = s.nonce
  , hash  = s.hash
  , user  = ()
  }

updateSession : Data.WithSession -> Session -> Session
updateSession s s' =
  { s' | nonce = s.nonce
       , hash  = s.hash
  }


type alias EveryState =
  { lastResponse : Bool -- whether the response was good
  , session      : Session
  }

type alias Model =
  { session    : Maybe Session
  , lastHash   : Maybe Data.Hashed
  , nonceState : Nonce.Model Msg
  , dataState  : Data.Model Msg
  , everyState : Every.Model EveryState
  }

nextSessionTick : Maybe EveryState -> Time -> Time
nextSessionTick me soFar =
  let standard = Time.second * 5
  in case me of
       Nothing -> standard
       Just e ->
         if e.lastResponse
         then standard
         else standard + (2 * soFar)

type Msg
  = Login User.Credentials
  | NonceMsg (Nonce.Msg Msg)
  | DataMsg  (Data.Msg Msg)
  | EveryMsg (Every.Msg EveryState)
  | LoginResponse Data.Hashed Data.WithSession
  | PingSession (Maybe EveryState)
  | PingSessionResponse Data.Hashed Data.WithSession
  | UpdateSession Session
  -- | GET String (Maybe JsonD.Value -> Cmd a)
  -- unpacks the `data` component

init : (Model, Cmd Msg)
init =
  let (nonceModel, nonceEff) = Nonce.init
      (dataModel,  dataEff)  = Data.init
  in  ( { session    = Nothing
        , lastHash   = Nothing
        , nonceState = nonceModel
        , dataState  = dataModel
        , everyState = Every.init
        }
      , Cmd.batch
          [ Cmd.map NonceMsg nonceEff
          , Cmd.map DataMsg dataEff
          ]
      )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    NonceMsg a ->
      let (newNonce, eff) = Nonce.update a model.nonceState
      in  ( { model | nonceState = newNonce }
          , Cmd.map (\r -> case r of
                             Err x -> NonceMsg x
                             Ok  x -> x) eff
          )
    DataMsg a ->
      let (newData, eff) = Data.update a model.dataState
      in  ( { model | dataState = newData }
          , Cmd.map (\r -> case r of
                             Err x -> DataMsg x
                             Ok  x -> x) eff
          )
    EveryMsg a ->
      let (newEvery, eff) = Every.update
                              nextSessionTick
                              (mkCmd << PingSession)
                              a
                              model.everyState
      in  ( { model | everyState = newEvery }
          , Cmd.map (\r -> case r of
                             Err a -> a
                             Ok  x -> EveryMsg x) eff
          )
    Login cs ->
      ( model
      , mkCmd <| NonceMsg <| Nonce.NewNonce <| \n ->
        let data = User.encodeCredentials cs
        in  mkCmd <| DataMsg <| Data.MkWithSession n data Nothing <| \s ->
            Task.perform
              ( \_ -> EveryMsg <| Every.Start
                        { resetSoFar = False
                        , modifyData = Just <| Every.Update <| \me ->
                            case me of -- exponential backoff
                              Nothing -> Nothing
                              Just e  -> Just { e | lastResponse = False }
                        }
              )
              (LoginResponse s.hash)
              <| Http.post
                   Data.decodeWithSession
                   "/login"
                   (Http.string <| JsonE.encode 0 <| Data.encodeWithSession s)
      )
    LoginResponse loginHash s ->
      case toSession s of
        Nothing -> Debug.crash "fak" -- TODO: Session warning messages
        Just s' ->
          ( model
          , mkCmd <| DataMsg <| Data.CkWithSession loginHash s <| \isLegit ->
              if isLegit
              then Cmd.batch
                     [ mkCmd <| UpdateSession s'
                     , mkCmd <| EveryMsg <| Every.Start
                                  { resetSoFar = True
                                  , modifyData = Just <| Every.Assign
                                                   { lastResponse = True
                                                   , session      = s'
                                                   }
                                  }
                     ]
              else Debug.log "bad login response" Cmd.none
          )
    PingSession me ->
      case me of
        Nothing ->
          ( model
          , mkCmd <| EveryMsg Every.Stop
          )
        Just e -> Debug.log "pinging..."
          ( model
          , let data = JsonE.string "ping"
            in  mkCmd <| DataMsg <| Data.MkWithSession
                                      e.session.nonce
                                      data
                                      (Just e.session.hash) <| \s ->
                Task.perform
                  ( \_ -> EveryMsg <| Every.Start
                            { resetSoFar = False
                            , modifyData = Just <| Every.Update <| \me ->
                                case me of -- exponential backoff
                                  Nothing -> Nothing
                                  Just e  -> Just { e | lastResponse = False }
                            }
                  )
                  (PingSessionResponse s.hash)
                  <| Http.post
                       Data.decodeWithSession
                       "/session"
                       (Http.string <| JsonE.encode 0 <| Data.encodeWithSession s)
          )
    PingSessionResponse lastHash s ->
      case model.session of
        Nothing -> Debug.crash "no session after ping"
        Just s' -> -- TODO: Session warning messages
          ( model
          , mkCmd <| DataMsg <| Data.CkWithSession lastHash s <| \isLegit ->
            if isLegit
            then let newSession = updateSession s s'
            in Cmd.batch
                 [ mkCmd <| UpdateSession newSession
                 , mkCmd <| EveryMsg <| Every.Start
                              { resetSoFar = True
                              , modifyData = Just <| Every.Assign
                                               { lastResponse = True
                                               , session = newSession
                                               }
                              }
                 ]
            else Debug.log "bad session hash" Cmd.none
          )
    UpdateSession s ->
      ( { model | session = Just s }
      , Cmd.none
      )

subscriptions : Sub Msg
subscriptions =
  Sub.batch
    [ Sub.map NonceMsg Nonce.subscriptions
    , Sub.map DataMsg  Data.subscriptions
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
