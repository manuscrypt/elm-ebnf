module Generator exposing (..)

import WsnParsers as Parsers exposing (Factor(..), Production, Syntax, Term)


sample : Parsers.Syntax
sample =
    Parsers.makeSyntax
        [ number, letter, line ]


number : Production
number =
    Production "number"
        [ [ Literal "0" ]
        , [ Literal "1" ]
        , [ Literal "2" ]
        , [ Literal "3" ]
        , [ Literal "4" ]
        , [ Literal "5" ]
        , [ Literal "6" ]
        , [ Literal "7" ]
        , [ Literal "8" ]
        , [ Literal "9" ]
        ]


letter : Production
letter =
    Production "letter"
        [ [ Literal "a" ]
        , [ Literal "b" ]
        , [ Literal "c" ]
        , [ Literal "d" ]
        , [ Literal "e" ]
        , [ Literal "f" ]
        , [ Literal "g" ]
        , [ Literal "h" ]
        , [ Literal "i" ]
        , [ Literal "j" ]
        , [ Literal "k" ]
        , [ Literal "l" ]
        , [ Literal "m" ]
        , [ Literal "n" ]
        , [ Literal "o" ]
        , [ Literal "p" ]
        , [ Literal "q" ]
        , [ Literal "r" ]
        , [ Literal "s" ]
        , [ Literal "t" ]
        , [ Literal "u" ]
        , [ Literal "v" ]
        , [ Literal "w" ]
        , [ Literal "x" ]
        , [ Literal "y" ]
        , [ Literal "z" ]
        ]


line : Production
line =
    Production "line"
        [ [ Id "number"
          , Literal "-"
          , Id "number"
          , Id "letter"
          , Literal ":"
          , Repetition [ [ Id "letter" ] ]
          ]
        ]


input : Production
input =
    Production "input" [ [ Repetition [ [ Id "line" ] ] ] ]


generate : Syntax -> String
generate syntax =
    Parsers.productions syntax
        |> List.map generateProduction
        |> String.join "\n"


generateProduction : Production -> String
generateProduction prod =
    prod.identifier
        ++ " = "
        ++ "oneOf [ "
        ++ (prod.expression
                |> List.map generateTerm
                |> List.intersperse ","
                |> String.join "\n"
           )
        ++ "]"


generateTerm : Term -> String
generateTerm term =
    term |> List.map generateFactor |> String.join "\n"


generateFactor : Factor -> String
generateFactor f =
    case f of
        Id str ->
            str

        Literal str ->
            "chompIf (\\c -> String.fromChar c ==\"" ++ str ++ "\")"

        Repetition exp ->
            exp |> List.map generateTerm |> String.join "\n"

        Option _ ->
            "option"

        Group _ ->
            "group"
