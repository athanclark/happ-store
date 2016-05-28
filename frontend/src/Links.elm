module Links exposing (..)


type Inclusion
  = All
  | Any

type SearchLink
  = Term String
  | Categories (List String) Inclusion

type Link
  = Home
  | Search (Maybe SearchLink)
  | Package String
