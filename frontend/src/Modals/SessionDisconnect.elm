module Modals.SessionDisconnect exposing
  ( Model
  , Msg (Open, UpdateTimeLeft, Close)
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



type alias Model a =
  { visibility : Bool
  , timeLeft   : Int
  , onRetry    : Cmd a
  , onLogout   : Cmd a
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
  | Close -- only called by the continuation
  | ClickedRetry
  | ClickedLogout
  | DecrementSecond

-- TODO: Change to Maybe?
init : (Model a, Cmd (Msg a))
init =
  ( { visibility  = False
    , timeLeft    = 0
    , onRetry     = Cmd.none
    , onLogout    = Cmd.none
    }
  , Cmd.none
  )

update : Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    Open o ->
      ( { model | visibility = True
                , timeLeft   = o.timeLeft
                , onRetry    = o.onRetry
                , onLogout   = o.onLogout
        }
      , Cmd.none
      )
    UpdateTimeLeft t ->
      ( { model | timeLeft = t }
      , Cmd.none
      )
    Close ->
      ( { model | visibility = False
                , timeLeft   = 0
        }
      , Cmd.none
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

view : Model a -> Html (Msg a)
view model =
  div [ class <| "ui basic modal"
          ++ if model.visibility
             then " visible active"
             else " hidden"
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
            , div [ class "ui purple inverted button"
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
  if model.visibility
  then Time.every Time.second <| \_ -> DecrementSecond
  else Sub.none
