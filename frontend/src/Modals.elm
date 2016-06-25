module Modals exposing (..)

import Modals.SessionDisconnect as SessionDisconnect

import Html.App        as Html
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Result
import IntDict exposing (IntDict)
import Cmd.Extra exposing (mkCmd)


type alias TaskId = Int

type ModalModel a
  = SessDiscoModel (SessionDisconnect.Model (Result (Msg a) a))

type ModalMsg a
  = SessDiscoMsg (SessionDisconnect.Msg (Result (Msg a) a))

type alias Model a =
  { opacity : Float
  , taskId  : TaskId
  , modals  : IntDict (ModalModel a)
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

init : (Model a, Cmd (Msg a))
init =
  ( { opacity = 0
    , taskId  = 0
    , modals  = IntDict.empty
    }
  , -- Cmd.none
    mkCmd <| SessionDisconnect { timeLeft = 10, onRetry = Cmd.none, onLogout = Cmd.none}
  )

update : Msg a -> Model a -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    SessionDisconnect o ->
      let (t, model') = newTaskId model
          (newSess, sessEff) = SessionDisconnect.init
      in  ( { model' | modals = IntDict.insert t (SessDiscoModel newSess) model'.modals
            }
          , Cmd.batch
              [ Cmd.map (Err << ModalMsg t << SessDiscoMsg) sessEff
              , mkCmd <| Err <| ModalMsg t <| SessDiscoMsg
                      <| SessionDisconnect.Open
                           { o | onRetry  = Cmd.map Ok o.onRetry
                               , onLogout = Cmd.batch
                                   [ Cmd.map Ok o.onLogout
                                   , mkCmd <| Err <| Remove t
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



view : Model a -> Html (Msg a)
view model =
  case IntDict.findMin model.modals of
    Nothing -> div [class "ui dimmer modals page transition hidden"] []
    Just (t,x) ->
      div [class "ui dimmer modals page transition visible active"]
        [ case x of
            SessDiscoModel m ->
              Html.map (ModalMsg t << SessDiscoMsg) <| SessionDisconnect.view m
        ]


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  Sub.batch <|
    List.map
      (\(t,m') -> case m' of
                   SessDiscoModel m -> Sub.map (ModalMsg t << SessDiscoMsg)
                                         <| SessionDisconnect.subscriptions m
      ) <| IntDict.toList model.modals
