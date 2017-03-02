module Types exposing (..)


type alias Identifier =
    String


type Rhs
    = Identifier String
    | Terminal String
    | Option Rhs
    | Repetition Rhs
    | Grouping Rhs
    | Alternation Rhs Rhs


type alias Rule =
    { lhs : String
    , rhs : Rhs
    }


type alias Grammar =
    List Rule
