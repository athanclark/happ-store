module Beliefs where

import Beliefs.Types   exposing (..)

import Effects         exposing (Effects)
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)


type BeliefsMode
  = BeliefsStatement
  | BeliefsExistence
  | BeliefsEndorsement
  | BeliefsMeta

type alias BeliefsModel =
  { beliefsStatements   : List Statement
  , beliefsExistences   : List Existence
  , beliefsEndorsements : List Endorsement
  , beliefsMetas        : List Meta
  , beliefsMode         : BeliefsMode
  }

initBeliefsModel : BeliefsModel
initBeliefsModel =
  { beliefsStatements   = []
  , beliefsExistences   = []
  , beliefsEndorsements = []
  , beliefsMetas        = []
  , beliefsMode         = BeliefsStatement
  }


type BeliefsAction
  = ClickedStatements
  | ClickedExistence
  | ClickedEndorsements
  | ClickedMetas

beliefsUpdate : BeliefsAction
             -> BeliefsModel
             -> (BeliefsModel, Effects BeliefsAction)
beliefsUpdate action model =
  case action of
    ClickedStatements ->
      ( { model | beliefsMode = BeliefsStatement
        }
      , Effects.none
      )
    ClickedExistence ->
      ( { model | beliefsMode = BeliefsExistence
        }
      , Effects.none
      )
    ClickedEndorsements ->
      ( { model | beliefsMode = BeliefsEndorsement
        }
      , Effects.none
      )
    ClickedMetas ->
      ( { model | beliefsMode = BeliefsMeta
        }
      , Effects.none
      )

beliefsView : Signal.Address BeliefsAction
           -> BeliefsModel
           -> List Html
beliefsView address model =
  [ h2 [class "ui dividing header"]
      [text "Share Your Beliefs"]
  , div [class "ui secondary pointing menu"]
      [ a [ class <| "item" ++ (case model.beliefsMode of
                                  BeliefsStatement -> " active"
                                  _                -> "")
          , onClick address ClickedStatements
          ] [text "Statements"]
      , a [ class <| "item" ++ (case model.beliefsMode of
                                  BeliefsExistence -> " active"
                                  _                -> "")
          , onClick address ClickedExistence
          ] [text "Existence"]
      , a [ class <| "item" ++ (case model.beliefsMode of
                                  BeliefsEndorsement -> " active"
                                  _                  -> "")
          , onClick address ClickedEndorsements
          ] [text "Endorsements"]
      , a [ class <| "item" ++ (case model.beliefsMode of
                                  BeliefsMeta -> " active"
                                  _           -> "")
          , onClick address ClickedMetas
          ] [text "Meta Beliefs"]
      ]
  ]
