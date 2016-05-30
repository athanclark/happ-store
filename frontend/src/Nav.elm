module Nav exposing (..)

import Links

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)


type alias Model =
  { currentPage : Links.Link
  , position    : Float
  , visibility  : Float
  }

type Msg
  = ChangePage Links.Link
  | ChangePosition Float
  | ChangeVisibility Float

init : (Model, Cmd Msg)
init =
  ( { currentPage = Links.Home
    , position    = 0
    , visibility  = 0
    }
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ChangePage l ->
      ( { model | currentPage = l }
      , Cmd.none
      )
    ChangePosition p ->
      ( { model | position = p }
      , Cmd.none
      )
    ChangeVisibility v ->
      ( { model | visibility = v }
      , Cmd.none
      )

view : Model -> List (Html Msg)
view model =
    [ a [ class "item header"
        , onClick <| ChangePage Links.Home
        ]
        [ text "hApp Store"
        ]
    ]


viewMobile : Model -> Html Msg
viewMobile model =
  div [ class <| "ui left vertical menu inverted sidebar"
          ++ if model.visibility > 0
             then " visible"
             else ""
      , style <|
          if model.visibility > 0
          then
            [ ( "opactiy"
              , toString model.visibility
              )
            , ( "left"
              , let length = 260
                in  toString (negate <| length -
                                (length * model.position))
                    ++ "px"
              )
            ]
          else []
      ]
    [ div [class "ui header item"] [text "hApp Store"]
    , a [ class <| "item"
            ++ case model.currentPage of
                 Links.Home -> " active"
                 _          -> ""
        , onClick <| ChangePage Links.Home
        ]
        [ i [class "home icon"] []
        , text "Home"
        ]
   -- , a [ class <| "item"
   --         ++ case model.currentPage of
   --              Types.Menu -> " active"
   --              _          -> ""
   --     , onClick <| ChangedPage Types.Menu
   --     ]
   --     [ i [class "shop icon"] []
   --     , text "Browse"
   --     ]
   -- , a [ class <| "item"
   --         ++ case model.currentPage of
   --              Types.News -> " active"
   --              _          -> ""
   --     , onClick <| ChangedPage Types.News
   --     ]
   --     [ i [class "newspaper icon"] []
   --     , text "News"
   --     ]
   -- , a [ class <| "item"
   --         ++ case model.currentPage of
   --              Types.FAQs -> " active"
   --              _          -> ""
   --     , onClick <| ChangedPage Types.FAQs
   --     ]
   --     [ i [class "help icon"] []
   --     , text "FAQ's"
   --     ]
   -- , a [ class <| "item"
   --         ++ case model.currentPage of
   --              Types.About -> " active"
   --              _           -> ""
   --     , onClick <| ChangedPage Types.About
   --     ]
   --     [ i [class "user icon"] []
   --     , text "About Us"
   --     ]
   -- , a [ class <| "item"
   --         ++ case model.currentPage of
   --              Types.Find -> " active"
   --              _          -> ""
   --     , onClick <| ChangedPage Types.Find
   --     ]
   --     [ i [class "world icon"] []
   --     , text "Locations"
   --     ]
   -- , a [ class <| "item"
   --         ++ case model.currentPage of
   --              Types.Talk -> " active"
   --              _          -> ""
   --     , onClick <| ChangedPage Types.Talk
   --     ]
   --     [ i [class "mail icon"] []
   --     , text "Contact Us"
   --     ]
    ]
