module Links exposing (..)

import Cabal


type Inclusion
  = All
  | Any

type SearchLink
  = Term String
  | Categories (List String) Inclusion

type Link
  = Home
  | Search (Maybe SearchLink)
  | Package Cabal.PackageName
  | Profile
  | NotFound

type alias ChangeLink a =
  { gotoHome    : Cmd a
  , gotoPackage : Cabal.PackageName -> Cmd a
  , gotoProfile : Cmd a
  , gotoLink    : Link -> Cmd a
  }
