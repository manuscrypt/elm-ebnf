module Parsers exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Set
import Symbols exposing (..)


type Syntax
    = Syntax (List Production)


type Production
    = Production Factor Expression


type alias Expression =
    List Factor


type Factor
    = Identifier String
    | Literal String
    | Option Expression
    | Group Expression
    | Repetition Expression
    | Alternation Expression


syntax : Parser Syntax
syntax =
    succeed Syntax
        |= repeat zeroOrMore production


production : Parser Production
production =
    succeed Production
        |. spaces
        |= identifier
        |. spaces
        |. symbol "="
        |= alternation
        |. oneOf [ symbol ".", symbol ";" ]
        |. spaces


expression : Parser (List Factor)
expression =
    repeat zeroOrMore
        (succeed identity
            |. spaces
            |= factor
            |. spaces
        )


alternation : Parser (List Factor)
alternation =
    succeed identity
        |= sequence
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = spaces
            , item = factor
            , trailing = Forbidden
            }


factor : Parser Factor
factor =
    oneOf
        [ identifier
        , literal
        , lazy (\_ -> oneOf [ opt, grp, rep ])
        ]


opt : Parser Factor
opt =
    succeed Option |= list spaces (lazy (\_ -> factor))


grp : Parser Factor
grp =
    succeed Group |= tuple spaces (lazy (\_ -> factor))


rep : Parser Factor
rep =
    succeed Repetition |= record spaces (lazy (\_ -> factor))


identifier : Parser Factor
identifier =
    succeed Identifier |= variable isLetter isCharacter Set.empty


literal : Parser Factor
literal =
    succeed Literal |= oneOf [ sLiteral, dLiteral ]


sLiteral : Parser String
sLiteral =
    succeed identity |. spaces |. symbol "'" |= anyString |. symbol "'"


dLiteral : Parser String
dLiteral =
    succeed identity |. symbol "\"" |= anyString |. symbol "\""


anyString : Parser String
anyString =
    oneOf
        [ succeed identity |= symbol "'" |> andThen (\_ -> succeed "'")
        , succeed identity |= symbol "\"" |> andThen (\_ -> succeed "\"")
        , succeed identity |= variable isCharacter isCharacter Set.empty
        ]



--
-- delayedCommit spaces <|
--     succeed identity
--         |. symbol "|"
--         |. spaces
--         |= lazy (\_ -> term)
