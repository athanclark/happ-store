module Beliefs.Types where

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Object =
  { objectName        : String
  }

objectIcon : Html
objectIcon = i [class "icon cube"] []

type alias Event =
  { eventTime : String
  , eventName : String
  }

eventIcon : Html
eventIcon = i [class "icon calendar"] []

viewEventCard : Event -> Html
viewEventCard event =
   div [class "content"]
     [ div [class "header"]
         [text <| event.eventName]
     , div [class "meta"]
         [text <| event.eventTime]
     ]


type Person
  = PersonKnown String -- UserId
  | PersonUnknown String

personIcon : Html
personIcon = i [class "icon user"] []

type Existence
  = ExistenceObject Object
  | ExistenceEvent  Event
  | ExistencePerson Person


viewExistence : List Html -> Existence -> Html
viewExistence content ex =
  div [class "ui raised segment"] <|
    case ex of
      ExistenceObject o ->
        [ div [class "ui header"]
            [ objectIcon
            , div [class "content"]
                [text <| o.objectName]
            ]
        , div [class "divider"] []
        ] ++ content
      ExistenceEvent e ->
        [ div [class "ui header"]
            [ eventIcon
            , div [class "content"]
                [ text <| e.eventName
                , div [class "sub header"]
                    [text <| e.eventTime]
                ]
            ]
        , div [class "divider"] []
        ] ++ content
      ExistencePerson p ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text <| case p of
                           PersonKnown x -> x
                           PersonUnknown x -> x]
            ]
        , div [class "divider"] []
        ] ++ content

type Subject
  = SubjectObject Object
  | SubjectEvent  Event
  | SubjectPerson Person

type alias Statement =
  { statementSubject   : Subject
  , statementStatement : String
  }

viewStatement : List Html -> Statement -> Html
viewStatement content st =
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
        , div [class "divider"] []
        ] ++ content
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
        , div [class "divider"] []
        ] ++ content
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
        , div [class "divider"] []
        ] ++ content


type alias Endorsement = Person

viewEndorsement : List Html -> Endorsement -> Html
viewEndorsement content person =
  div [class "ui raised segment"] <|
    case person of
      PersonKnown x ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text x]
            ]
        , div [class "divider"] []
        ] ++ content
      PersonUnknown x ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text x]
            ]
        , div [class "divider"] []
        ] ++ content

type Belief
  = BeliefExistence   Existence
  | BeliefStatement   Statement
  | BeliefEndorsement Endorsement

type alias Meta =
  { metaSubject : Person
  , metaBelief  : Belief
  }

metaIcon : Html
metaIcon = i [class "icon settings"] []

viewMeta : List Html -> Meta -> Html
viewMeta content meta =
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
        BeliefExistence e -> viewExistence [] e
        BeliefStatement s -> viewStatement [] s
        BeliefEndorsement e -> viewEndorsement [] e
    , div [class "divider"] []
    ] ++ content
