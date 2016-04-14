module Beliefs where

import Beliefs.Types   exposing (..)

import Dict            exposing (Dict)
import Effects         exposing (Effects)
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)


type BeliefsMode
  = BeliefsStatement
  | BeliefsExistence
  | BeliefsEndorsement
  | BeliefsMeta

type alias BeliefId = String

type alias BeliefsModel =
  { beliefsStatements   : Dict BeliefId (Statement   , Maybe Int, Maybe Int)
  , beliefsExistences   : Dict BeliefId (Existence   , Maybe Int, Maybe Int)
  , beliefsEndorsements : Dict BeliefId (Endorsement , Maybe Int, Maybe Int)
  , beliefsMetas        : Dict BeliefId (Meta        , Maybe Int, Maybe Int)
  , beliefsMode         : BeliefsMode
  }

initBeliefsModel : BeliefsModel
initBeliefsModel =
  { beliefsStatements   = Dict.fromList <| [
        ( "psychosis"
        , ({
          statementSubject = SubjectObject {objectName = "Foo"}
        , statementStatement = "yolo"
        }, Nothing, Nothing))
      ]
  , beliefsExistences   = Dict.fromList <| [
        ( "bar"
        , (ExistenceObject {objectName = "Foo"}, Just 10, Just 10))
      ]
  , beliefsEndorsements = Dict.fromList <| [
        ( "baz"
        , (PersonUnknown "Asdf", Nothing, Nothing)
        )
      ]
  , beliefsMetas        = Dict.fromList <| [
        ( "foo"
        , ({ metaSubject = PersonUnknown "Me lol"
        , metaBelief = BeliefEndorsement (PersonUnknown "You hahah")
        , metaMeasure = (20,40)
        }, Nothing, Nothing))
      ]
  , beliefsMode         = BeliefsStatement
  }

type BeliefsAction
  = ClickedStatements
  | ClickedExistence
  | ClickedEndorsements
  | ClickedMetas
  | BeliefAction BeliefId (BeliefAction)

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
    BeliefAction k a ->
      case model.beliefsMode of
        BeliefsStatement ->
          let (new, eff) = case Dict.get k model.beliefsStatements of
                               Nothing -> Debug.crash <| "Belief nonexistent: `" ++ k ++ "`"
                               Just x  -> beliefUpdate a x
          in  ( { model | beliefsStatements =
                            Dict.insert k
                                        new
                                        model.beliefsStatements
                }
              , Effects.map (BeliefAction k) eff
              )
        BeliefsExistence ->
          let (new, eff) = case Dict.get k model.beliefsExistences of
                             Nothing -> Debug.crash <| "Belief nonexistent: `" ++ k ++ "`"
                             Just x  -> beliefUpdate a x
          in  ( { model | beliefsExistences =
                    Dict.insert k
                                new
                                model.beliefsExistences
                }
              , Effects.map (BeliefAction k) eff
              )
        BeliefsEndorsement ->
          let (new, eff) = case Dict.get k model.beliefsEndorsements of
                             Nothing -> Debug.crash <| "Belief nonexistent: `" ++ k ++ "`"
                             Just x  -> beliefUpdate a x
          in  ( { model | beliefsEndorsements =
                    Dict.insert k
                                new
                                model.beliefsEndorsements
                }
              , Effects.map (BeliefAction k) eff
              )
        BeliefsMeta ->
          let (new, eff) = case Dict.get k model.beliefsMetas of
                             Nothing -> Debug.crash <| "Belief nonexistent: `" ++ k ++ "`"
                             Just x  -> beliefUpdate a x
          in  ( { model | beliefsMetas =
                    Dict.insert k
                                new
                                model.beliefsMetas
                }
              , Effects.map (BeliefAction k) eff
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
  ] ++ (case model.beliefsMode of
          BeliefsStatement ->
            List.map (\(k,x) -> viewStatement
                                  (Signal.forwardTo address <| BeliefAction k)
                                  True
                                  x)
              <| Dict.toList model.beliefsStatements
          BeliefsExistence ->
            List.map (\(k,x) -> viewExistence
                                  (Signal.forwardTo address <| BeliefAction k)
                                  True
                                  x)
              <| Dict.toList model.beliefsExistences
          BeliefsEndorsement ->
            List.map (\(k,x) -> viewEndorsement
                                  (Signal.forwardTo address <| BeliefAction k)
                                  True
                                  x)
              <| Dict.toList model.beliefsEndorsements
          BeliefsMeta ->
            List.map (\(k,x) -> viewMeta
                                  (Signal.forwardTo address <| BeliefAction k)
                                  x)
              <| Dict.toList model.beliefsMetas
       )
