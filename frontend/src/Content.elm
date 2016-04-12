module Content where

import Beliefs         exposing (BeliefsModel, initBeliefsModel)
import People          exposing (PeopleModel, initPeopleModel)

import Effects         exposing (Effects)
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)


type alias ContentModel =
  { contentBeliefs : BeliefsModel
  , contentPeople  : PeopleModel
  , contentMode    : ContentMode
  }

type ContentMode
  = ContentBeliefs
  | ContentPeople

initContentModel =
  { contentBeliefs = initBeliefsModel
  , contentPeople  = initPeopleModel
  , contentMode    = ContentBeliefs
  }

type ContentAction
  = ClickedBeliefs
  | ClickedPeople

contentUpdate : ContentAction
             -> ContentModel
             -> (ContentModel, Effects ContentAction)
contentUpdate action model =
  case action of
    ClickedBeliefs -> ( { model | contentMode = ContentBeliefs
                        }
                      , Effects.none
                      )
    ClickedPeople  -> ( { model | contentMode = ContentPeople
                        }
                      , Effects.none
                      )


contentView : Signal.Address ContentAction
           -> ContentModel
           -> List Html
contentView address model =
  [ div [class "row"]
      [ div [class "sixteen wide column"]
          [ div [class "ui top attached tabular menu"]
              [ a [ class <| "item" ++ (case model.contentMode of
                                          ContentBeliefs -> " active"
                                          _              -> "")
                  , onClick address ClickedBeliefs
                  ]
                  [text "Beliefs"]
              , a [ class <| "item" ++ (case model.contentMode of
                                          ContentPeople -> " active"
                                          _             -> "")
                  , onClick address ClickedPeople
                  ]
                  [text "People"]
              ]
          , div [class "ui bottom attached segment"]
              [ text "yo"
              ]
          ]
      ]
  ]
