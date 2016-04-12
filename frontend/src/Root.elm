module Root where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects)

type alias RootModel subModel =
  { rootNav : NavModel
  , rootContent : subModel
  }

initRootModel : m -> RootModel m
initRootModel initContentModel =
  { rootNav = initNavModel
  , rootContent = initContentModel
  }

type RootAction subAction
  = NavAction NavAction
  | ContentAction subAction


rootUpdate : (a -> m -> (m, Effects a))
          -> RootAction a
          -> RootModel m
          -> (RootModel m, Effects (RootAction a))
rootUpdate contentUpdate action model =
  case action of
    NavAction nav ->
      let (rootNav', navEffs) = navUpdate nav model.rootNav
      in  ({ model | rootNav = rootNav' }, Effects.map NavAction navEffs)
    ContentAction content ->
      let (rootContent', contentEffs) = contentUpdate content model.rootContent
      in  ({ model | rootContent = rootContent' }, Effects.map ContentAction contentEffs)


rootTemplate : (Signal.Address a -> m -> List Html)
            -> Signal.Address (RootAction a)
            -> RootModel m
            -> Html
rootTemplate contentView address model =
  div []
    [ topNav (Signal.forwardTo address NavAction) model.rootNav
    , div [ class "ui grid container"
          , id "content"
          ] <|
        (contentView (Signal.forwardTo address ContentAction) model.rootContent)
        ++ [ footer
           ]
    ]

footer : Html
footer =
  div [class "one column row"]
    [ div [class "column center aligned"]
        [text "Copyright (c) 2016 Athan Clark"]
    ]


-- Navigation stuff

type alias NavUser =
  { navUsername : String
  }

type alias NavModel =
  { navUser : Maybe NavUser
  }

initNavModel : NavModel
initNavModel =
  { navUser = Nothing
  }

type NavAction
  = ClickedLogin
  | ClickedRegister
  | ClickedUser


navUpdate : NavAction
         -> NavModel
         -> (NavModel, Effects NavAction)
navUpdate action model =
  case action of
    ClickedLogin ->
      (model, Effects.none)
    ClickedRegister ->
      (model, Effects.none)
    ClickedUser ->
      (model, Effects.none)

topNav : Signal.Address NavAction
      -> NavModel
      -> Html
topNav address model =
  div []
    [ div [class "ui top fixed menu"]
        [ div [class "header item"]
            [ text "Cooperate"
            ]
        , div [class "right menu"] <|
            case model.navUser of
              Nothing ->
                [ a [ class "item"
                    , onClick address ClickedLogin
                    ]
                    [text "Login"]
                , a [ class "item"
                    , onClick address ClickedRegister
                    ]
                    [text "Register"]
                ]
              Just user ->
                [ div [ class "item"
                      , onClick address ClickedUser
                      ]
                    [text <| "Hello, " ++ user.navUsername ++ "!"]
                ]
        ]
    ]
