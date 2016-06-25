module Modals exposing (..)

import Modals.SessionDisconnect as SessionDisconnect

import Html.App        as Html
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Result
import IntDict exposing (IntDict)
import Cmd.Extra exposing (mkCmd)
import Duration
import Time exposing (Time, millisecond)
import Ease


type alias TaskId = Int

type ModalModel a
  = SessDiscoModel (SessionDisconnect.Model (Result (Msg a) a))

type ModalMsg a
  = SessDiscoMsg (SessionDisconnect.Msg (Result (Msg a) a))

type alias Model a =
  { visibility : Float
  , taskId     : TaskId
  , modals     : IntDict (ModalModel a)
  , duration   : Duration.Model (Msg a)
  }


newTaskId : Model a -> (TaskId, Model a)
newTaskId model = (model.taskId, { model | taskId = model.taskId + 1 })

type Msg a
  = SessionDisconnect { timeLeft : Int
                      , onRetry  : Cmd a
                      , onLogout : Cmd a
                      }
  | ModalMsg TaskId (ModalMsg a)
  | Remove TaskId
  | ChangeVisibility Float
  | DurationMsg (Duration.Msg (Msg a))
  | Open
  | Close

init : (Model a, Cmd (Msg a))
init =
  ( { visibility = 0
    , taskId     = 0
    , modals     = IntDict.empty
    , duration   = Duration.init
    }
  , -- Cmd.none
    mkCmd <| SessionDisconnect { timeLeft = 10, onRetry = Cmd.none, onLogout = Cmd.none}
  )

update : Msg a -> Model a -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    Open ->
      ( model
      , mkCmd <| Err <| DurationMsg <| Duration.Forward <| \_ -> Cmd.none
      )
    Close ->
      ( model
      , mkCmd <| Err <| DurationMsg <| Duration.Reverse <| \_ -> Cmd.none
      )
    SessionDisconnect o ->
      let (t, model') = newTaskId model
          (newSess, sessEff) = SessionDisconnect.init
      in  ( { model' | modals = IntDict.insert t (SessDiscoModel newSess) model'.modals
            }
          , Cmd.batch
              [ Cmd.map (Err << ModalMsg t << SessDiscoMsg) sessEff
              , if IntDict.isEmpty model.modals
                then mkCmd <| Err Open
                else Cmd.none
              , mkCmd <| Err <| ModalMsg t <| SessDiscoMsg
                      <| SessionDisconnect.Open
                           { o | onRetry  = Cmd.map Ok o.onRetry
                               , onLogout = Cmd.batch
                                   [ Cmd.map Ok o.onLogout
                                   , Cmd.batch
                                       [ if IntDict.isEmpty <| IntDict.remove t
                                                                 model.modals
                                         then mkCmd <| Err Close
                                         else Cmd.none
                                       , mkCmd <| Err <| ModalMsg t <| SessDiscoMsg <|
                                           SessionDisconnect.Close <|
                                             mkCmd <| Err <| Remove t
                                       ]
                                   ]
                           }
              ]
          )
    ModalMsg t a ->
      case IntDict.get t model.modals of
        Nothing -> (model, Cmd.none)
        Just m ->
          case (a,m) of
            (SessDiscoMsg a', SessDiscoModel m') ->
              let (newSess, sessEff) = SessionDisconnect.update a' m'
              in  ( { model | modals = IntDict.insert t (SessDiscoModel newSess)
                                         model.modals
                    }
                  , Cmd.map (\r -> case r of
                                     Err a -> Err <| ModalMsg t <| SessDiscoMsg a
                                     Ok a  -> a) sessEff
                  )
           -- _ -> (model, Cmd.none)
    Remove t ->
      ( { model | modals = IntDict.remove t model.modals }
      , Cmd.none
      )
    ChangeVisibility v ->
      ( { model | visibility = v }
      , Cmd.none
      )
    DurationMsg a ->
      let timeLength = 500 * millisecond
          (newDur, eff) = Duration.update
                            (\t -> mkCmd <| ChangeVisibility <|
                                     Ease.outQuad <| t / timeLength
                            )
                            timeLength
                            a
                            model.duration
      in  ( { model | duration = newDur }
          , Cmd.map (\r -> case r of
                             Err a -> Err a
                             Ok a  -> Err a) eff
          )



view : Model a -> Html (Msg a)
view model =
  let mx = IntDict.findMin model.modals
  in  div [ class <| "ui dimmer modals page transition"
                       ++ case mx of
                            Nothing -> " hidden"
                            Just _  -> " visible active"
          , style [("opacity", toString model.visibility)]
          ] <|
        case mx of
          Nothing -> []
          Just (t,x) ->
            [ case x of
                SessDiscoModel m ->
                  Html.map (ModalMsg t << SessDiscoMsg) <| SessionDisconnect.view m
            ]


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  Sub.batch <|
    ( List.map
        (\(t,m') -> case m' of
                     SessDiscoModel m -> Sub.map (ModalMsg t << SessDiscoMsg)
                                           <| SessionDisconnect.subscriptions m
        ) <| IntDict.toList model.modals
    ) ++ [ Sub.map DurationMsg <| Duration.subscriptions model.duration ]
