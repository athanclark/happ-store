module Beliefs.Types where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as A

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


viewExistence : Bool -> Existence -> Html
viewExistence renderFooter ex =
  div [class "ui raised segment"] <|
    case ex of
      ExistenceObject o ->
        [ div [class "ui header"]
            [ objectIcon
            , div [class "content"]
                [text <| o.objectName]
            ]
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Exists", "Exclusion")
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
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Did, or Will Happen", "Exclusion")
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
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("They Exist", "Exclusion")
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

viewStatement : Bool -> Statement -> Html
viewStatement renderFooter st =
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
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Agreement", "Exclusion")
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
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Agreement", "Exclusion")
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
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Agreement", "Exclusion")
                      ]]
              else [])


type alias Endorsement = Person

viewEndorsement : Bool -> Endorsement -> Html
viewEndorsement renderFooter person =
  div [class "ui raised segment"] <|
    case person of
      PersonKnown x ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text x]
            ]
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Endorsement", "Exclusion")
                      ]]
              else [])
      PersonUnknown x ->
        [ div [class "ui header"]
            [ personIcon
            , div [class "content"]
                [text x]
            ]
        , div [class "divider"] []
        ] ++ (if renderFooter
              then [div [class "ui grid"]
                      [ div [class "two column row"] <|
                          opinionFooter ("Endorsement", "Exclusion")
                      ]]
              else [])

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

viewMeta : Meta -> Html
viewMeta meta =
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
        BeliefExistence e   -> viewExistence   False e
        BeliefStatement s   -> viewStatement   False s
        BeliefEndorsement e -> viewEndorsement False e
    , div [class "divider"] []
    , div [class "ui grid"]
        [ div [class "two column row"] <|
            opinionFooter ("They Do Believe", "Exclusion")
        ]
    ]


opinionFooter : (String, String)
             -> List Html
opinionFooter (l,r) =
  [ div [class "column"]
      [ div [class "ui blue label"]
          [text l]
      , input [ type' "range"
              , value "125"
              , A.min "0"
              , A.max "256"
              ] []
      ]
  , div [class "column"]
      [ div [class "ui red label"]
          [text r]
      , input [ type' "range"
              , value "125"
              , A.min "0"
              , A.max "256"
              ] []
      ]
  ]

