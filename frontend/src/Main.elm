module Main exposing (..)

import Nav
import Cmd.Extra exposing (mkCmd)

import Window
import Navigation
import Duration
import Ease

import Task
import Time exposing (Time, millisecond)


import Html.App        as App
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)




type ResponsiveScreen
  = Mobile
  | Tablet
  | Small
  | Large

isMobile : ResponsiveScreen -> Bool
isMobile s =
  case s of
    Mobile -> True
    Tablet -> True
    _      -> False


type alias Model =
  { nav         : Nav.Model
  , screenSize  : ResponsiveScreen
  , sidebarDuration : Duration.Model Msg
  }

type Msg
  = NavMsg Nav.Msg
  | ChangedScreenWidth Int
  | DurationMsg (Duration.Msg Msg)


init : (Model, Cmd Msg)
init =
  let (newNav, navEff) = Nav.init
  in  ( { nav         = newNav
        , screenSize  = Mobile
        , sidebarDuration = Duration.init
        }
      , Cmd.batch
          [ Cmd.map NavMsg navEff
          , Task.perform
              Debug.crash
              ChangedScreenWidth
              Window.width
          ]
      )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    NavMsg a ->
      let (newNav, eff) = Nav.update a model.nav
      in  ( { model | nav = newNav }
          , Cmd.map NavMsg eff
          )
    DurationMsg a ->
      let timeLength = 500 * millisecond
          (newDur, eff) = Duration.update
                            (\t -> Cmd.batch
                                     [ mkCmd <| NavMsg <| Nav.ChangePosition <|
                                         Ease.outQuad <| t / timeLength
                                     , mkCmd <| NavMsg <| Nav.ChangeVisibility <|
                                         Ease.outQuad <| t / timeLength
                                     ]
                            )
                            timeLength
                            a
                            model.sidebarDuration
      in  ( { model | sidebarDuration = newDur }
          , Cmd.map (\r -> case r of
                             Err x -> x
                             Ok  x -> x) eff
          )
    ChangedScreenWidth w ->
      ( { model | screenSize =
            if w > 1200
            then Large
            else if w > 992
            then Small
            else if w > 768
            then Tablet
            else Mobile
        }
      , Cmd.none
      )

view : Model -> Html Msg
view model =
  let mobileSidebar = App.map NavMsg <| Nav.viewMobile model.nav
      mobileMenuButton =
        a [ class "item"
          , onClick <| DurationMsg <|
              case model.sidebarDuration.elapsed of
                Just _  -> Duration.Toggle <| \_ -> Cmd.none
                Nothing ->
                  if model.nav.position == 0
                  then Duration.Forward <| \_ -> Cmd.none
                  else Duration.Reverse <| \_ -> Cmd.none
          ]
          [i [class "icon sidebar"] []]
  in  div [] <|
        ( if isMobile model.screenSize
          then [mobileSidebar]
          else []
        ) ++
        [ div [ class "ui top fixed menu"
              , style <|
                  [ ( "border-bottom"
                    , "2px solid violet"
                    )
                  ] ++ if model.nav.visibility > 0
                       then [ ( "left"
                              , let length = 260
                                in  toString (negate <| length -
                                                (length * model.nav.position))
                                    ++ "px"
                              )
                            ]
                       else []
              ] <|
            if isMobile model.screenSize
            then [mobileMenuButton]
            else List.map (App.map NavMsg) (Nav.view model.nav)
        , div [ class "pusher"
              , style <|
                  if model.nav.visibility > 0
                  then [ ("position", "relative")
                       , ( "left"
                         , let length = 260
                           in  toString (negate <| length -
                                           (length * model.nav.position))
                               ++ "px"
                         )
                       ]
                  else []
              ]
            [ div [class "full height"]
                [ div [ style [ ( "margin-top"
                                , if isMobile model.screenSize
                                  then "4em"
                                  else "4em"
                                )
                              ]
                      ]
                    []
                , div [class "ui container grid"]
                    [ div [class "sixteen wide column"]
                        [ div [class "ui segment"]
                            [ text "Foo"
                            ]
                        ]
                    ]
                ]
            ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\x -> ChangedScreenWidth x.width)
    , Sub.map DurationMsg <| Duration.subscriptions model.sidebarDuration
    ]


main : Program Never
main =
  App.program
     { init = init
     , update = update
     , view = view
     , subscriptions = subscriptions
     }
