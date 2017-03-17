module TryTwo exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Symbols exposing (..)
import Set


type alias Syntax =
    { title : Factor
    , productions : List Production
    }


type alias Production =
    { name : Factor
    , exp : Expression
    }


type alias Expression =
    { terms : List Term }


type alias Term =
    { factors : List Factor }


type Factor
    = Identifier String
    | Literal String
    | Option Expression
    | Group Expression
    | Repetition Expression


syntax : Parser Syntax
syntax =
    succeed Syntax
        |= title
        |. symbol "{"
        |= zeroOrMore production
        |. symbol "}"


production : Parser Production
production =
    succeed Production
        |= identifier
        |. symbol "="
        |= expression
        |. symbol "."


expression : Parser Expression
expression =
    succeed Expression |= terms


terms : Parser (List Term)
terms =
    zeroOrMore term


term : Parser Term
term =
    succeed Term |= zeroOrMore factor


factor : Parser Factor
factor =
    oneOf [ identifier, literal, opt, grp, rep ]


opt : Parser Factor
opt =
    succeed Option |. symbol "[" |. spaces |= (lazy (\_ -> expression)) |. spaces |. symbol "]"


grp : Parser Factor
grp =
    succeed Group |. symbol "(" |. spaces |= (lazy (\_ -> expression)) |. spaces |. symbol ")"


rep : Parser Factor
rep =
    succeed Repetition |. symbol "{" |. spaces |= (lazy (\_ -> expression)) |. spaces |. symbol "}"


identifier : Parser Factor
identifier =
    succeed Identifier |= variable isLetter isCharacter Set.empty


anyString : Parser String
anyString =
    variable isCharacter isCharacter Set.empty


title : Parser Factor
title =
    literal


comment : Parser Factor
comment =
    literal


literal : Parser Factor
literal =
    succeed Literal |= oneOf [ sLiteral, dLiteral ]


sLiteral : Parser String
sLiteral =
    succeed identity |. symbol "'" |= anyString |. symbol "'"


dLiteral : Parser String
dLiteral =
    succeed identity |. symbol "\"" |= anyString |. symbol "\""



--
-- delayedCommit spaces <|
--     succeed identity
--         |. symbol "|"
--         |. spaces
--         |= lazy (\_ -> term)
