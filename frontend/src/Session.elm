module Session exposing
  ( Model
  , Msg (Login, GET)
  , init
  , update
  , subscriptions
  , viewMenuItem
  )

import Session.Data  as Data
import User

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Every
import Http
import Json.Decode as JsonD exposing (Decoder, (:=))
import Json.Encode as JsonE
import Task exposing (Task)
import Task.Extra as Task
import Time exposing (Time)
import Cmd.Extra exposing (mkCmd)



signedPost : String -> Data.SignedRequest -> Task Http.Error Data.SignedResponse
signedPost url signedReq =
  Http.post
    Data.signedResponseDecoder
    url
    <| Http.string <| JsonE.encode 0
                   <| Data.encodeSignedRequest signedReq


type alias Session =
  { user  : ()
  }


makeSession : Maybe String -> Maybe Session
makeSession ms =
  case ms of
    Nothing -> Nothing
    Just s  -> if s == JsonE.encode 0 (JsonE.string "login success!")
               then Just { user = () }
               else Nothing


type alias EveryState =
  { lastResponse : Bool -- whether the response was good
  }

type alias Model a =
  { session    : Maybe Session
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
  | Logout
  | LoginResponse (Maybe LoginError -> Cmd a)
                  (Result LoginError Data.SignedResponse)
  | GET String (Maybe JsonE.Value) (Result SessionError JsonD.Value -> Cmd a)
  | GETResponse (Result SessionError JsonD.Value -> Cmd a)
                (Result SessionError Data.SignedResponse)
  | GETUnauthResponse (Result SessionError JsonD.Value -> Cmd a)
                      (Result SessionError JsonD.Value)
  | PingSession EveryState
  | PingSessionResponse (Result SessionError Data.SignedResponse)
  | UpdateSession Session
  | DataMsg  (Data.Msg (Result (Msg a) a))
  | EveryMsg (Every.Msg EveryState)
  -- unpacks the `data` component

init : (Model a, Cmd (Msg a))
init =
  let (dataModel,  dataEff)  = Data.init
  in  ( { session    = Nothing
        , dataState  = dataModel
        , everyState = Every.init { lastResponse = True }
        }
      , Cmd.map DataMsg dataEff
      )

update : (SessionError -> Cmd a)
      -> Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update onPingFail action model =
  case action of
    DataMsg a ->
      let (newData, eff) = Data.update a model.dataState
      in  ( { model | dataState = newData }
          , Cmd.map (\r -> case r of
                             Err x -> Err <| DataMsg x
                             Ok x  -> x) eff
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
      , let message = JsonE.encode 0 <| User.encodeCredentials cs
        in  mkCmd <| Err <| DataMsg <| Data.Sign message <| \signedReq ->
            Cmd.map (Err << LoginResponse onLogin) <|
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
                  <| signedPost "/login" signedReq
      )
    LoginResponse onLogin eResponse ->
      case eResponse of
        Err e -> (model, Cmd.map Ok <| onLogin <| Just e)
        Ok signedResp ->
          ( model
          , mkCmd <| Err <| DataMsg <| Data.Open signedResp <| \mResponse ->
              case makeSession mResponse of
                Nothing -> Cmd.map Ok <| onLogin <| Just LoginMalicious
                Just session ->
                  Cmd.batch
                    [ mkCmd <| Err <| UpdateSession session
                    , mkCmd <| Err <| EveryMsg <| Every.Start
                            <| \_ -> { lastResponse = True
                                     }
                    , Cmd.map Ok <| onLogin Nothing
                    ]
          )
    Logout ->
      ( { model | session = Nothing }
      , mkCmd <| Err <| EveryMsg Every.Stop
      )
    PingSession e ->
      ( model
      , let message = Debug.log "pinging..." <| "ping"
        in  mkCmd <| Err <| DataMsg <| Data.Sign message <| \signedReq ->
            Cmd.map (Err << PingSessionResponse) <| Task.perform
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
              <| signedPost "/session" signedReq
      )
    PingSessionResponse eResponse ->
      case eResponse of
        Err e -> (model, Cmd.map Ok <| onPingFail e)
        Ok signedResp ->
          case model.session of
            Just s' -> -- TODO: Session warning messages
              ( model
              , mkCmd <| Err <| DataMsg <| Data.Open signedResp <| \mResponse ->
                case mResponse of
                  Nothing -> Cmd.map Ok <| onPingFail <| SessionMalicious
                  Just response ->
                    if response /= "pong"
                    then Cmd.map Ok <| onPingFail <| SessionMalicious
                    else mkCmd <| Err <| EveryMsg <| Every.Adjust
                           { reset = True -- was sucessful
                           , modify = \_ -> { lastResponse = True
                                            }
                           }
              )
            Nothing ->
              ( model -- fail silently; already logged out
              , Cmd.none
              )
    GET location data handler ->
      ( model
      , let handleError e = case e of
                              Http.NetworkError -> Err SessionNetworkError
                              Http.Timeout -> Err SessionTimeout
                              Http.BadResponse c _ ->
                                if c == 403 || c == 400
                                then Err SessionMalicious
                                else if c == 404
                                then Err SessionExpired
                                else Err SessionNetworkError
                              Http.UnexpectedPayload _ -> Err SessionNetworkError
        in case model.session of
             Nothing ->
               Cmd.map (Err << GETUnauthResponse handler) <| Task.perform
                 handleError
                 Ok
                 <| Http.get
                      JsonD.value
                      location
             Just s' ->
               mkCmd <| Err <| DataMsg
                    <| Data.Sign (JsonE.encode 0 <| Maybe.withDefault (JsonE.list []) data)
                    <| \signedReq ->
               Cmd.map (Err << GETResponse handler) <| Task.perform
                 handleError
                 Ok
                 <| signedPost location signedReq
      )
    GETResponse handler eResponse ->
      case eResponse of
        Err e -> (model, Cmd.map Ok <| handler <| Err e)
        Ok signedResp ->
          ( model
          , mkCmd <| Err <| DataMsg <| Data.Open signedResp <| \mResponse ->
              case mResponse of -- TODO: Fix sign_open errors
                Nothing -> Cmd.map Ok <| handler <| Err SessionMalicious
                Just response ->
                  case JsonD.decodeString JsonD.value response of
                    Err e -> Cmd.map Ok <| handler <| Err SessionMalicious
                             -- really just not formatted right
                    Ok x -> Cmd.batch
                              [ mkCmd <| Err <| EveryMsg <| Every.Adjust
                                  { reset = True -- was sucessful
                                  , modify = \_ -> { lastResponse = True
                                                   }
                                  }
                              , Cmd.map Ok <| handler <| Ok x
                              ]
          )
    GETUnauthResponse handler es ->
      ( model
      , Cmd.map Ok <| handler es
      )
    UpdateSession s ->
      ( { model | session = Just s }
      , Cmd.none
      )


subscriptions : Sub (Msg a)
subscriptions =
  Sub.map DataMsg  Data.subscriptions


-- login button
viewMenuItem : Model a -> Html (Msg a)
viewMenuItem model =
  a [ class "item"
    , onClick <| case model.session of
                   Nothing -> Login (User.Plain { username = "", password = "" })
                                    (\_ -> Cmd.none)
                   Just _ -> Logout
    ] <|
    case model.session of
      Nothing -> [ i [class "icon sign in"] []
                 , text "Login"
                 ]
      Just u  -> [text "sup dawg!"]
