module Parsers exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
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
        |= repeat oneOrMore production
        |. end


production : Parser Production
production =
    inContext "production" <|
        succeed Production
            |. spaces
            |= identifier
            |. spaces
            |. symbol "="
            |. spaces
            |= rhs
            |. spaces
            |. oneOf [ symbol ".", symbol ";" ]


rhs : Parser Rhs
rhs =
    inContext "rhs" <|
        succeed identity
            |= oneOf
                [ concatenation
                , alternation
                , succeed Single
                    |= lazy
                        (\_ -> expression)
                ]


expression : Parser Expression
expression =
    inContext "expression" <|
        succeed identity
            |. spaces
            |= oneOf
                [ succeed RefId |= identifier
                , literal
                , lazy
                    (\_ ->
                        oneOf
                            [ opt
                            , grp
                            , rep
                            ]
                    )
                ]
            |. spaces


alternation : Parser Rhs
alternation =
    inContext "alternation" <|
        succeed Alternation
            |= lazy
                (\_ -> expression)
            |. symbol "|"
            |= lazy
                (\_ -> expression)



-- repeat zeroOrMore
--       |= sequence
--           { start = ""
--           , separator = "|"
--           , end = ""
--           , spaces = spaces
--           , item = expression
--           , trailing = Forbidden
--           }


concatenation : Parser Rhs
concatenation =
    inContext "concatenation" <|
        succeed Concatenation
            |= lazy
                (\_ -> expression)
            |. symbol ","
            |= lazy
                (\_ -> expression)



-- succeed Concatenation
--     |= sequence
--         { start = ""
--         , separator = ","
--         , end = ""
--         , spaces = spaces
--         , item = expression
--         , trailing = Forbidden
--         }


opt : Parser Expression
opt =
    inContext "option" <|
        succeed Option
            |= list spaces (lazy (\_ -> expression))


grp : Parser Expression
grp =
    inContext "group" <|
        succeed Group
            |= tuple spaces (lazy (\_ -> expression))


rep : Parser Expression
rep =
    inContext "repetition" <|
        succeed Repetition
            |= record spaces (lazy (\_ -> expression))


identifier : Parser Identifier
identifier =
    inContext "identifier" <|
        succeed Identifier
            |= variable isLetter isLetterOrDigitOrUnderscore Set.empty


literal : Parser Expression
literal =
    inContext "literal" <|
        succeed Literal
            |= oneOf [ sLiteral, dLiteral ]


sLiteral : Parser String
sLiteral =
    succeed identity |. symbol "'" |= anyString |. symbol "'"


dLiteral : Parser String
dLiteral =
    succeed identity |. symbol "\"" |= anyString |. symbol "\""


anyString : Parser String
anyString =
    oneOf
        [ source (ignore (Exactly 1) (\c -> c == '\''))
        , source (ignore (Exactly 1) (\c -> c == '"'))
        , variable isCharacter isCharacter Set.empty
        ]



--
-- delayedCommit spaces <|
--     succeed identity
--         |. symbol "|"
--         |. spaces
--         |= lazy (\_ -> term)
