module Beliefs.Types where

import String exposing (toInt)
import Effects exposing (Effects)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as A
import Html.Attributes.Extra exposing (valueAsInt)
import Html.Events exposing (..)


type BeliefAction
  = ChangedMeasure Int
  | ChangedExclusion Int
  | ClickedSubmit

beliefUpdate : BeliefAction
            -> (a, Maybe Int, Maybe Int)
            -> ((a, Maybe Int, Maybe Int), Effects BeliefAction)
beliefUpdate action (x,mmeasure, mexclusion) =
  case action of
    ChangedMeasure n -> Debug.log (toString n) <| ((x, Just n, mexclusion), Effects.none)
    ChangedExclusion n -> ((x, mmeasure, Just n), Effects.none)
    ClickedSubmit -> ((x,mmeasure, mexclusion), Effects.none)

type alias Object =
  { objectName : String
  }

objectIcon : Html
objectIcon = i [class "icon cube"] []

type alias Event =
  { eventTime : String
  , eventName : String
  }

eventIcon : Html
eventIcon = i [class "icon calendar"] []


type Person
  = PersonKnown String -- UserId
  | PersonUnknown String

personIcon : Html
personIcon = i [class "icon user"] []

type Existence
  = ExistenceObject Object
  | ExistenceEvent  Event
  | ExistencePerson Person


viewExistence : Signal.Address BeliefAction
             -> Bool
             -> (Existence, Maybe Int, Maybe Int)
             -> Html
viewExistence address renderFooter (ex, mval, mexp) =
  div [class "ui raised segment"] <|
    case ex of
      ExistenceObject o ->
        [ div [class "ui header"]
            [ objectIcon
            , div [class "content"]
                [text <| o.objectName]
            ]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Exists", "Exclusion") mval mexp
                      ]]
              else [])
      ExistenceEvent e ->
        [ div [class "ui header"]
            [ eventIcon
            , div [class "content"]
                [ text <| e.eventName
                , div [class "sub header"]
                    [text <| e.eventTime]
                ]
            ]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Did, or Will Happen", "Exclusion") mval mexp
                      ]]
              else [])
      ExistencePerson p ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text <| case p of
                           PersonKnown x -> x
                           PersonUnknown x -> x]
            ]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("They Exist", "Exclusion") mval mexp
                      ]]
              else [])

type Subject
  = SubjectObject Object
  | SubjectEvent  Event
  | SubjectPerson Person

type alias Statement =
  { statementSubject   : Subject
  , statementStatement : String
  }

viewStatement : Signal.Address BeliefAction
             -> Bool
             -> (Statement, Maybe Int, Maybe Int)
             -> Html
viewStatement address renderFooter (st, mval, mexp) =
  div [class "ui raised segment"] <|
    case st.statementSubject of
      SubjectObject o ->
        [ div [class "ui header"]
            [ objectIcon
            , div [class "content"]
                [text <| o.objectName]
            ]
        , div []
            [text <| st.statementStatement]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Agreement", "Exclusion") mval mexp
                      ]]
              else [])
      SubjectEvent e ->
        [ div [class "ui header"]
            [ eventIcon
            , div [class "content"]
                [ text <| e.eventName
                , div [class "sub header"]
                    [text <| e.eventTime]
                ]
            ]
        , div []
            [text <| st.statementStatement]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Agreement", "Exclusion") mval mexp
                      ]]
              else [])
      SubjectPerson p ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text <| case p of
                           PersonKnown x -> x
                           PersonUnknown x -> x]
            ]
        , div []
            [text <| st.statementStatement]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Agreement", "Exclusion") mval mexp
                      ]]
              else [])


type alias Endorsement = Person

viewEndorsement : Signal.Address BeliefAction
               -> Bool
               -> (Endorsement, Maybe Int, Maybe Int)
               -> Html
viewEndorsement address renderFooter (person, mval, mexp) =
  div [class "ui raised segment"] <|
    case person of
      PersonKnown x ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text x]
            ]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Endorsement", "Exclusion") mval mexp
                      ]]
              else [])
      PersonUnknown x ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text x]
            ]
        , div [class "ui divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "row"] <|
                          opinionFooter address ("Endorsement", "Exclusion") mval mexp
                      ]]
              else [])

type Belief
  = BeliefExistence   Existence
  | BeliefStatement   Statement
  | BeliefEndorsement Endorsement


type alias Meta =
  { metaSubject : Person
  , metaBelief  : Belief
  , metaMeasure : (Int, Int)
  }

metaIcon : Html
metaIcon = i [class "icon settings"] []

viewMeta : Signal.Address BeliefAction
        -> (Meta, Maybe Int, Maybe Int)
        -> Html
viewMeta address (meta, mval, mexp) =
  div [class "ui raised segment"] <|
    [ div [class "ui header"]
        [ metaIcon
        , div [class "content"]
            [ text <| case meta.metaSubject of
                        PersonKnown x -> x
                        PersonUnknown x -> x
            , div [class "sub header"]
                [text "May believe..."]
            ]
        ]
    , case meta.metaBelief of
        BeliefExistence e   -> viewExistence  address False (e, Nothing, Nothing)
        BeliefStatement s   -> viewStatement  address False (s, Nothing, Nothing)
        BeliefEndorsement e -> viewEndorsement address False (e, Nothing, Nothing)
    , div [class "ui grid two column"]
        [ input [ type' "range"
                , A.min "0"
                , A.max "255"
                , A.step "1"
                , disabled True
                , value <| toString <| fst meta.metaMeasure
                ] []
        , input [ type' "range"
                , A.min "0"
                , A.max "255"
                , A.step "1"
                , disabled True
                , value <| toString <| snd meta.metaMeasure
            ] []
        ]
    , div [class "ui divider"] []
    , div [class "ui grid"]
        [ div [class "row"] <|
            opinionFooter address ("They Feel This Way", "Exclusion") mval mexp
        ]
    ]

opinionFooter : Signal.Address BeliefAction
             -> (String, String)
             -> Maybe Int
             -> Maybe Int
             -> List Html
opinionFooter address (l,r) mval mexp =
  [ div [class "six wide column"]
      [ div [class "ui header"]
          [text l]
      , input [ type' "range"
              , value <| toString <| Maybe.withDefault 125 mval
              , A.min "0"
              , A.max "255"
              , A.step "1"
              , on "change" targetValue
                  <| \x -> Signal.message address
                        <| ChangedMeasure
                        <| case toInt x of
                             Err e -> Debug.crash e
                             Ok x' -> x'
              ] []
      ]
  , div [class "six wide column"]
      [ div [class "ui red header"]
          [text r]
      , input [ type' "range"
              , value <| toString <| Maybe.withDefault 0 mexp
              , A.min "0"
              , A.max "255"
              , A.step "1"
              , on "change" targetValue
                  <| \x -> Signal.message address
                        <| ChangedExclusion
                        <| case toInt x of
                             Err e -> Debug.crash e
                             Ok x' -> x'
              ] []
      ]
  , div [class "four wide column bottom aligned"]
      [ input [ type' "submit"
              , value "Save"
              , class <| "ui fluid button" ++ (case (mval, mexp) of
                                                 (Nothing, Nothing) -> " disabled"
                                                 _ -> " green")
              ] []
      ]
  ]

