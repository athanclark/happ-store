module Modals.SessionDisconnect exposing
  ( Model
  , Msg (Open, UpdateTimeLeft, Close, ChangeVisibility)
  , init
  , update
  , view
  , subscriptions
  )

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Time exposing (Time)
import Colors
import Color
import Duration
import Ease
import Time exposing (Time, millisecond)
import Cmd.Extra exposing (mkCmd)


type alias Model a =
  { visibility : Float
  , timeLeft   : Int
  , onRetry    : Cmd a
  , onLogout   : Cmd a
  , duration   : Duration.Model a
  }

type alias OpenParams a =
  { timeLeft : Int
  , onRetry  : Cmd a
  , onLogout : Cmd a
  }


-- some modals may provide data on open
type Msg a
  = Open (OpenParams a)
  | UpdateTimeLeft Int
  | Close (Cmd a) -- only called by the continuation
  | ClickedRetry
  | ClickedLogout
  | DecrementSecond
  | ChangeVisibility Float
  | DurationMsg (Duration.Msg a)

-- TODO: Change to Maybe?
init : (Model a, Cmd (Msg a))
init =
  ( { visibility  = 0
    , timeLeft    = 0
    , onRetry     = Cmd.none
    , onLogout    = Cmd.none
    , duration    = Duration.init
    }
  , Cmd.none
  )

update : Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    Open o ->
      ( { model | timeLeft   = o.timeLeft
                , onRetry    = o.onRetry
                , onLogout   = o.onLogout
        }
      , mkCmd <| Err <| DurationMsg <| Duration.Forward <| \_ -> Cmd.none
      )
    UpdateTimeLeft t ->
      ( { model | timeLeft = t }
      , Cmd.none
      )
    Close onComplete ->
      ( { model | timeLeft   = 0
        }
      , mkCmd <| Err <| DurationMsg <| Duration.Reverse <| \_ -> onComplete
      )
    ClickedRetry ->
      ( model
      , Cmd.map Ok model.onRetry
      )
    ClickedLogout ->
      ( model
      , Cmd.map Ok model.onLogout
      )
    DecrementSecond ->
      ( { model | timeLeft = model.timeLeft - 1 }
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
          , eff
          )


view : Model a -> Html (Msg a)
view model =
  div [ class <| "ui basic modal"
          ++ if model.visibility > 0
             then " visible active"
             else " hidden"
      , style [("opacity", toString model.visibility)]
      ]
    [ div [class "header"] [text "Session Disconnected"]
    , div [class "content"]
        [ p []
            [ text <| "Finna retry in "
                   ++ toString model.timeLeft
                   ++ " seconds, ya dig...?" ]
        ]
    , div [class "actions"]
        [ div [class "two fluid ui inverted buttons"]
            [ div [ class "ui red inverted button"
                  , onClick ClickedLogout
                  ]
                [ i [class "icon sign out"] []
                , text "Logout"
                ]
            , div [ class "ui basic purple inverted button"
                  , onClick ClickedRetry
                  ]
                [ i [class "icon repeat"] []
                , text "Retry Now"
                ]
            ]
        ]
    ]

subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  Sub.batch
    [ if model.visibility > 0
      then Time.every Time.second <| \_ -> DecrementSecond
      else Sub.none
    , Sub.map DurationMsg <| Duration.subscriptions model.duration
    ]
