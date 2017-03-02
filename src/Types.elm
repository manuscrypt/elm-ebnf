module Types exposing (..)


type Rhs
    = Identifier String
    | Terminal String
    | Option Rhs
    | Repetition Rhs
    | Grouping Rhs
    | Alternation Rhs Rhs
    | Concatenation Rhs Rhs
    | NoRhs


type alias Rule =
    { lhs : String
    , rhs : Rhs
    }


type alias Grammar =
    List Rule
