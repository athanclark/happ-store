module Beliefs.Types where


type alias Object = String

type alias Event =
  { eventTime : String
  , eventName : String
  }

type Person
  = PersonKnown String -- UserId
  | PersonUnknown String


type Existence
  = ExistenceObject Object
  | ExistenceEvent  Event
  | ExistencePerson Person

type Subject
  = SubjectObject Object
  | SubjectEvent  Event
  | SubjectPerson Person

type alias Statement =
  { statementSubject   : Subject
  , statementStatement : String
  }

type alias Endorsement = Person


type Belief
  = BeliefExistence   Existence
  | BeliefStatement   Statement
  | BeliefEndorsement Endorsement

type alias Meta =
  { metaSubject : Person
  , metaBelief  : Belief
  }
