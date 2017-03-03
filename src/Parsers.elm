module Parsers exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Symbols exposing (..)
import Types exposing (..)
import Set


grammarParser : Parser (List Rule)
grammarParser =
    succeed identity
        |= Parser.zeroOrMore rule
        |. end


rule : Parser Rule
rule =
    succeed Rule
        |. spaces
        |= identifier
        |. spaces
        |. keyword "="
        |. spaces
        |= oneOf rhs
        |. spaces
        |. keyword ";"


identifier : Parser String
identifier =
    variable isLetter isLetterOrDigitOrUnderscore noKeywords


terminal : Parser Rhs
terminal =
    succeed Terminal
        |= oneOf [ singleQuotedString, doubleQuotedString ]


noKeywords : Set.Set comparable
noKeywords =
    Set.fromList []


rhs : List (Parser Rhs)
rhs =
    [ lazy (\_ -> alternation)
    , lazy (\_ -> concatenation)
    , lazy (\_ -> option)
    , lazy (\_ -> grouping)
    , lazy (\_ -> repetition)
    , terminal
    , ident
    ]


ident : Parser Rhs
ident =
    succeed Identifier |= identifier


concatenation : Parser Rhs
concatenation =
    succeed Concatenation
        |= oneOf rhs
        |= con


alternation : Parser Rhs
alternation =
    succeed Alternation
        |= oneOf rhs
        |= alt


alt : Parser Rhs
alt =
    delayedCommit spaces <|
        succeed identity
            |. symbol "|"
            |. spaces
            |= oneOf rhs


con : Parser Rhs
con =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= oneOf rhs


option : Parser Rhs
option =
    succeed Option
        |. keyword "["
        |. spaces
        |= oneOf rhs
        |. spaces
        |. keyword "]"


grouping : Parser Rhs
grouping =
    succeed Grouping
        |. keyword "("
        |. spaces
        |= oneOf rhs
        |. spaces
        |. keyword ")"


repetition : Parser Rhs
repetition =
    succeed Repetition
        |. keyword "{"
        |. spaces
        |= oneOf rhs
        |. spaces
        |. keyword "}"


comment : Parser String -> Parser Rhs
comment c =
    succeed Comment
        |. symbol "(*"
        |. spaces
        |= c
        |. spaces
        |. symbol "*)"


keySymbols : Set.Set String
keySymbols =
    Set.fromList [ "=", ",", ";", "|", "-" ]


doubleQuotedString : Parser String
doubleQuotedString =
    variable (\c -> c == '"') (\c -> isCharacter c) (Set.fromList [])


singleQuotedString : Parser String
singleQuotedString =
    variable (\c -> c == '\'') (\c -> isCharacter c) (Set.fromList [])



-- special =
--      succeed identity
--          |. symbol "?"
--          |. spaces
--          |= seq
--          |. spaces
--          |. symbol "?"
