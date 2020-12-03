module BnfParsers exposing (Expression(..), Identifier(..), Production(..), Syntax(..), anyString, dLiteral, expression, grp, identifier, opt, production, rep, sLiteral, syntax)

--import Parser.Advanced as Parser exposing (..)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), backtrackable, chompIf, chompUntilEndOr, chompWhile, end, getChompedString, lazy, loop, map, oneOf, sequence, spaces, succeed, symbol, variable)
import Set
import Symbols exposing (isCharacter, isLetter, isLetterOrDigitOrUnderscore, isSymbol)


type Syntax
    = Syntax (List Production)


type Identifier
    = Identifier String


type Production
    = Production Identifier (List (List Expression))


type Expression
    = RefId Identifier
    | Literal String
    | Option (List Expression)
    | Group (List Expression)
    | Repetition (List Expression)


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
        |= definition
        |. spaces
        |. oneOf [ symbol ".", symbol ";" ]


definition : Parser (List (List Expression))
definition =
    sequence
        { start = ""
        , separator = "|"
        , end = ""
        , spaces = spaces
        , item = term
        , trailing = Optional
        }


term : Parser (List Expression)
term =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = expression
        , trailing = Optional
        }



--|. chompUntilEndOr "\n"


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



-- type Rhs
--     = Alternation (List Expression)
--     | Concatenation (List Expression)
--     | Single Expression
-- rhs : Parser Rhs
-- rhs =
--     succeed identity
--         |= oneOf
--             [ backtrackable concatenation
--             , backtrackable alternation
--             , backtrackable <| succeed Single |= expression
--             ]
-- concatenation : Parser Rhs
-- concatenation =
--     succeed Concatenation
--         |= expression
--         |. spaces
--         |. symbol ","
--         |. spaces
--         |= expression
--         |. spaces
-- alternation : Parser Rhs
-- alternation =
--     succeed Alternation
--         |= expression
--         |. spaces
--         |. symbol "|"
--         |. spaces
--         |= expression
--         |. spaces
-- concatenation : Parser Rhs
-- concatenation =
--     succeed Concatenation
--         |= sequence
--             { start = ""
--             , separator = ","
--             , end = ""
--             , spaces = spaces
--             , item = expression
--             , trailing = Optional
--             }
-- alternation : Parser Rhs
-- alternation =
--     succeed Alternation
--         |= sequence
--             { start = ""
--             , separator = "|"
--             , end = ""
--             , spaces = spaces
--             , item = expression
--             , trailing = Optional
--             }


opt : Parser Expression
opt =
    succeed Option
        |= sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = lazy (\_ -> expression), trailing = Optional }


grp : Parser Expression
grp =
    succeed Group
        |= sequence { start = "(", separator = ",", end = ")", spaces = spaces, item = lazy (\_ -> expression), trailing = Optional }



-- rep : Parser Expression
-- rep =
--     succeed Repetition
--         |= sequence { start = "{", separator = ",", end = "}", spaces = spaces, item = lazy (\_ -> expression), trailing = Optional }


rep : Parser Expression
rep =
    succeed Repetition
        |. symbol "{"
        |= lazy (\_ -> term)
        |. symbol "}"


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
