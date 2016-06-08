module Modals.SessionDisconnect exposing
  ( Model
  , Msg (Open, Update, Close)
  , init
  , update
  , view
  )

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)

import Time exposing (Time)



type alias Model a =
  { visibility  : Bool
  , secondsToGo : Int
  , cont        : Cmd a -- single threaded, no need for threads
  }

-- some modals may provide data on open
type Msg a
  = Open { delay    : Int
         , onRetry  : Cmd a
         , onLogout : Cmd a
         }
  | Update { delay : Int
           }
  | Close -- only called by the continuation
  | ClickedRetry
  | ClickedLogout
  | DecrementSecond

-- TODO: Change to Maybe?
init : (Model a, Cmd (Msg a))
init =
  ( { visibility  = False
    , secondsToGo = 0
    , cont        = Cmd.none
    }
  , Cmd.none
  )

update : Msg a -> Model a -> (Model a, Cmd (Result (Msg a) a))
update action model =
  case action of
    Open { onRetry } ->
      ( { model | visibility = True
                , cont       = onRetry
        }
      , Cmd.none
      )
    Close ->
      ( { model | visibility = False }
      , Cmd.none
      )
    ClickedRetry ->
      ( model
      , Cmd.map Ok model.cont
      )
    DecrementSecond ->
      ( { model | secondsToGo = model.secondsToGo - 1 }
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
            [ text "Gonna retry in a few..."]
        ]
    , div [class "actions"]
        [ div [class "two fluid ui inverted buttons"]
            [ div [class "ui blue basic inverted button"]
                [ i [class "icon sign out"] []
                , text "Logout"
                ]
            , div [class "ui violet basic inverted button"]
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
