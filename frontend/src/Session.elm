module Session exposing
  ( Model
  , Msg (Login, GET)
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

-- Works for any payload
updateSession : Data.WithSession -> Session -> Session
updateSession s s' =
  { s' | nonce = s.nonce
       , hash  = s.hash
  }


type alias EveryState =
  { lastResponse : Bool -- whether the response was good
  }

type alias Model a =
  { session    : Maybe Session
  , lastHash   : Maybe Data.Hashed
  , nonceState : Nonce.Model (Result (Msg a) a)
  , dataState  : Data.Model (Result (Msg a) a)
  , everyState : Every.Model EveryState
  }

nextSessionTick : EveryState -> Time -> Time
nextSessionTick e soFar =
  let standard = Time.second * 5
  in if e.lastResponse
  then standard
  else Debug.log "waiting for" <| standard + (2 * soFar)

type LoginError
  = LoginMalicious
  | LoginBadCredentials
  | LoginTimeout
  | LoginNetworkError

type SessionError
  = SessionMalicious
  | SessionExpired
  | SessionTimeout
  | SessionNetworkError

type Msg a
  = Login User.Credentials (Maybe LoginError -> Cmd a)
  | LoginResponse Data.Hashed (Maybe LoginError -> Cmd a)
                              (Result LoginError Data.WithSession)
  | GET String (Maybe JsonE.Value) (Result SessionError JsonD.Value -> Cmd a)
  | GETResponse Data.Hashed (Result SessionError JsonD.Value -> Cmd a)
                            (Result SessionError Data.WithSession)
  | PingSession EveryState
  | PingSessionResponse Data.Hashed (Result SessionError Data.WithSession)
  | UpdateSession Session
  | NonceMsg (Nonce.Msg (Result (Msg a) a))
  | DataMsg  (Data.Msg (Result (Msg a) a))
  | EveryMsg (Every.Msg EveryState)
  -- unpacks the `data` component

init : (Model a, Cmd (Msg a))
init =
  let (nonceModel, nonceEff) = Nonce.init
      (dataModel,  dataEff)  = Data.init
  in  ( { session    = Nothing
        , lastHash   = Nothing
        , nonceState = nonceModel
        , dataState  = dataModel
        , everyState = Every.init { lastResponse = True }
        }
      , Cmd.batch
          [ Cmd.map NonceMsg nonceEff
          , Cmd.map DataMsg dataEff
          ]
      )

update : (SessionError -> Cmd a)
      -> Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update onPingFail action model =
  case action of
    NonceMsg a ->
      let (newNonce, eff) = Nonce.update a model.nonceState
      in  ( { model | nonceState = newNonce }
          , Cmd.map (\r -> case r of
                             Err x -> Err <| NonceMsg x
                             Ok x  -> x) eff
          )
    DataMsg a ->
      let (newData, eff) = Data.update a model.dataState
      in  ( { model | dataState = newData }
          , Cmd.map (\r -> case r of
                             Err x -> Err <| DataMsg x
                             Ok x -> x) eff
          )
    EveryMsg a ->
      let (newEvery, eff) = Every.update
                              nextSessionTick
                              (mkCmd << Err << PingSession)
                              a
                              model.everyState
      in  ( { model | everyState = newEvery }
          , Cmd.map (\r -> case r of
                             Err x -> x
                             Ok x  -> Err <| EveryMsg x) eff
          )
    Login cs onLogin ->
      ( model
      , mkCmd <| Err <| NonceMsg <| Nonce.NewNonce <| \n ->
        let data = User.encodeCredentials cs
        in  mkCmd <| Err <| DataMsg <| Data.MkWithSession n data Nothing <| \s ->
            Cmd.map (Err << LoginResponse s.hash onLogin) <|
              Task.perform
                  (\e -> case e of
                           Http.NetworkError ->
                             Err LoginNetworkError
                           Http.Timeout ->
                             Err LoginTimeout
                           Http.BadResponse c _ ->
                             if c == 403 || c == 400
                             then Err LoginMalicious
                             else Err LoginBadCredentials
                           Http.UnexpectedPayload _ ->
                             Err LoginBadCredentials
                  )
                  Ok
                  <| Http.post
                        Data.decodeWithSession
                        "/login"
                        (Http.string <| JsonE.encode 0 <| Data.encodeWithSession s)
      )
    LoginResponse loginHash onLogin es ->
      case es of
        Err e -> (model, Cmd.map Ok <| onLogin <| Just e)
        Ok s ->
          case toSession s of
            Nothing -> Debug.crash "fak" -- TODO: Session warning messages
            Just s' ->
              ( model
              , mkCmd <| Err <| DataMsg <| Data.CkWithSession loginHash s <| \isLegit ->
                  if isLegit
                  then Cmd.batch
                          [ mkCmd <| Err <| UpdateSession s'
                          , mkCmd <| Err <| EveryMsg <| Every.Start
                                  <| \_ -> { lastResponse = True
                                           }
                          , Cmd.map Ok <| onLogin Nothing
                          ]
                  else Cmd.map Ok <| onLogin <| Just LoginMalicious
              )
    PingSession e ->
      ( model
      , let data = JsonE.string "ping"
        in  mkCmd <| Err <| DataMsg <| (Debug.log "pinging..." <| Data.MkWithSession
                                  (case model.session of
                                     Nothing -> Debug.crash "ping called without session!"
                                     Just s  -> s.nonce)
                                  data
                                  (Maybe.map .hash model.session)) <| \s ->
            Cmd.map (Err << PingSessionResponse s.hash) <| Task.perform
              (\e -> case e of
                       Http.NetworkError -> Err SessionNetworkError
                       Http.Timeout -> Err SessionTimeout
                       Http.BadResponse c _ ->
                         if c == 403 || c == 400
                         then Err SessionMalicious
                         else if c == 404
                         then Err SessionExpired
                         else Err SessionNetworkError
                       Http.UnexpectedPayload _ -> Err SessionNetworkError
              )
              Ok
              <| Http.post
                   Data.decodeWithSession
                   "/session"
                   (Http.string <| JsonE.encode 0 <| Data.encodeWithSession s)
      )
    PingSessionResponse lastHash es ->
      case es of
        Err e -> (model, Cmd.map Ok <| onPingFail e)
        Ok s ->
          case model.session of
            Just s' -> -- TODO: Session warning messages
              ( model
              , mkCmd <| Err <| DataMsg <| Data.CkWithSession lastHash s <| \isLegit ->
                if isLegit
                then let newSession = updateSession s s'
                in Cmd.batch
                    [ mkCmd <| Err <| UpdateSession newSession
                    , mkCmd <| Err <| EveryMsg <| Every.Adjust
                                  { reset = True -- was sucessful
                                  , modify = \_ -> { lastResponse = True
                                                  }
                                  }
                    ]
                else Cmd.map Ok <| onPingFail <| SessionMalicious
              )
            Nothing ->
              ( model -- fail silently; already logged out
              , Cmd.none
              )
    UpdateSession s ->
      ( { model | session = Just s }
      , Cmd.none
      )
    GET location data handler ->
      ( model
      , case model.session of
          Nothing -> Debug.crash "no session" -- TODO: Use Http.Get
          Just s' ->
            mkCmd <| Err <| DataMsg <| (Debug.log "pinging..." <| Data.MkWithSession
                              s'.nonce
                              (Maybe.withDefault (JsonE.list []) data)
                              (Maybe.map .hash model.session)) <| \s ->
            Cmd.map (Err << GETResponse s'.hash handler) <| Task.perform
              (\e -> case e of
                       Http.NetworkError -> Err SessionNetworkError
                       Http.Timeout -> Err SessionTimeout
                       Http.BadResponse c _ ->
                         if c == 403 || c == 400
                         then Err SessionMalicious
                         else if c == 404
                         then Err SessionExpired
                         else Err SessionNetworkError
                       Http.UnexpectedPayload _ -> Err SessionNetworkError
              )
              Ok
              <| Http.post
                    Data.decodeWithSession
                    location
                    (Http.string <| JsonE.encode 0 <| Data.encodeWithSession s)
      )
    GETResponse lastHash handler es ->
      case es of
        Err e -> (model, Cmd.map Ok <| handler <| Err e)
        Ok s ->
          ( model
          , case model.session of
            Nothing -> Debug.crash "no session" -- TODO: Fixme
            Just s' ->
              mkCmd <| Err <| DataMsg <| Data.CkWithSession lastHash s <| \isLegit ->
                let newSession = updateSession s s'
                in if isLegit
                then Cmd.batch
                        [ mkCmd <| Err <| UpdateSession newSession
                        , mkCmd <| Err <| EveryMsg <| Every.Adjust
                                    { reset = True -- was sucessful
                                    , modify = \_ -> { lastResponse = True
                                                    }
                                    }
                        , Cmd.map Ok <| handler <| Ok s.data
                        ]
                else Cmd.map Ok <| handler <| Err SessionMalicious
          )


subscriptions : Sub (Msg a)
subscriptions =
  Sub.batch
    [ Sub.map NonceMsg Nonce.subscriptions
    , Sub.map DataMsg  Data.subscriptions
    ]


viewMenuItem : Model a -> Html (Msg a)
viewMenuItem model =
  a [ class "item"
    , onClick <| Login (User.Plain { username = "", password = "" }) (\_ -> Cmd.none)
    ]
    [ case model.session of
        Nothing -> text "login"
        Just u  -> text "sup dawg!"
    ]
