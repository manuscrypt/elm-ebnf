module Parsers exposing (Expression(..), Identifier(..), Production(..), Rhs(..), Syntax(..), alternation, anyString, concatenation, dLiteral, expression, grp, identifier, opt, production, rep, sLiteral, syntax)

--import Parser.Advanced as Parser exposing (..)

import Parser exposing (..)
import Set
import Symbols exposing (..)


type Syntax
    = Syntax (List Production)


type Identifier
    = Identifier String


type Production
    = Production Identifier Rhs


type Expression
    = RefId Identifier
    | Literal String
    | Option (List Expression)
    | Group (List Expression)
    | Repetition (List Expression)


type Rhs
    = Alternation Expression Expression
    | Concatenation Expression Expression
    | Single Expression


syntax : Parser Syntax
syntax =
    succeed Syntax
        |= loop [] productionsHelp
        |. end


productionsHelp : List Production -> Parser (Step (List Production) (List Production))
productionsHelp revProductions =
    oneOf
        [ succeed (\prods -> Loop (prods :: revProductions))
            |= production
        , succeed ()
            |> map (\_ -> Done (List.reverse revProductions))
        ]


production : Parser Production
production =
    succeed Production
        |. spaces
        |= identifier
        |. spaces
        |. symbol "="
        |. spaces
        |= oneOf
            [ succeed Single |= expression
            , alternation
            , concatenation
            ]
        |. spaces
        |. oneOf [ symbol ".", symbol ";" ]
        |. chompUntilEndOr "\n"


expression : Parser Expression
expression =
    succeed identity
        |= oneOf
            [ succeed RefId |= identifier
            , succeed Literal
                |= oneOf [ sLiteral, dLiteral ]
            , oneOf
                [ opt
                , grp
                , rep
                ]
            ]


concatenation : Parser Rhs
concatenation =
    succeed Concatenation
        |. spaces
        |= expression
        |. spaces
        |. symbol ","
        |. spaces
        |= expression
        |. spaces


alternation : Parser Rhs
alternation =
    succeed Alternation
        |. spaces
        |= expression
        |. spaces
        |. symbol "|"
        |. spaces
        |= expression
        |. spaces


opt : Parser Expression
opt =
    succeed Option
        |= sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = lazy (\_ -> expression), trailing = Optional }


grp : Parser Expression
grp =
    succeed Group
        |= sequence { start = "(", separator = ",", end = ")", spaces = spaces, item = lazy (\_ -> expression), trailing = Optional }


rep : Parser Expression
rep =
    succeed Repetition
        |= sequence { start = "{", separator = ",", end = "}", spaces = spaces, item = lazy (\_ -> expression), trailing = Optional }


identifier : Parser Identifier
identifier =
    succeed Identifier
        |= variable { start = isLetter, inner = isLetterOrDigitOrUnderscore, reserved = Set.empty }


sLiteral : Parser String
sLiteral =
    succeed identity |. symbol "'" |= anyString |. symbol "'"


dLiteral : Parser String
dLiteral =
    succeed identity |. symbol "\"" |= anyString |. symbol "\""


anyString : Parser String
anyString =
    oneOf
        [ chompIf (\c -> c == '\'') |> getChompedString
        , chompIf (\c -> c == '"') |> getChompedString
        , variable { start = isCharacter, inner = isCharacter, reserved = Set.empty }
        ]



-- oneOf
--     [ symbol "\"" |. chompUntil "\"" |> getChompedString
--     , symbol "'" |. chompUntil "'" |> getChompedString
--     , variable { start = isCharacter, inner = isCharacter, reserved = Set.empty }
--     ]
--
-- delayedCommit spaces <|
--     succeed identity
--         |. symbol "|"
--         |. spaces
--         |= lazy (\_ -> term)
