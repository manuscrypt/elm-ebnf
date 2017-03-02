module Parsers exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Symbols exposing (..)
import Types exposing (..)
import Set


keySymbols : Set.Set String
keySymbols =
    Set.fromList [ "=", ",", ";", "|", "-" ]


singleQuote : Parser ()
singleQuote =
    keyword "'"


doubleQuote : Parser ()
doubleQuote =
    keyword "\""


noKeywords : Set.Set comparable
noKeywords =
    Set.fromList []


grammarParser : Parser (List Rule)
grammarParser =
    succeed identity
        |= Parser.list spaces rule
        |. end


rule : Parser Rule
rule =
    succeed Rule
        |= identifier
        |. spaces
        |. keyword "="
        |. spaces
        |= lazy (\_ -> rhs)
        |. spaces
        |. keyword ";"
        |. spaces
        |. end


identifier : Parser String
identifier =
    variable isLetter isLetterOrDigitOrUnderscore noKeywords


rhs : Parser Rhs
rhs =
    oneOf rhsOpts


rhsOpts : List (Parser Rhs)
rhsOpts =
    [ ident
    , terminal
    , option
    , repetition
    , grouping
    , alternation
    , concatenation
    , fail "no can do"
    ]


ident : Parser Rhs
ident =
    succeed Identifier |= identifier


terminal : Parser Rhs
terminal =
    succeed Terminal
        |= oneOf
            [ singleQuotedString
            , doubleQuotedString
            ]


singleQuotedString : Parser String
singleQuotedString =
    succeed identity
        |. singleQuote
        |= variable isCharacter isCharacter noKeywords
        |. singleQuote


doubleQuotedString : Parser String
doubleQuotedString =
    succeed identity
        |. doubleQuote
        |= variable isCharacter isCharacter noKeywords
        |. doubleQuote


option : Parser Rhs
option =
    succeed Option
        |. keyword "["
        |. spaces
        |= lazy (\_ -> rhs)
        |. spaces
        |. keyword "]"


repetition : Parser Rhs
repetition =
    succeed Repetition
        |. keyword "{"
        |. spaces
        |= lazy (\_ -> rhs)
        |. spaces
        |. keyword "}"


grouping : Parser Rhs
grouping =
    succeed Grouping
        |. keyword "("
        |. spaces
        |= lazy (\_ -> rhs)
        |. spaces
        |. keyword ")"


enclosedIn : String -> String -> Parser Rhs
enclosedIn open close =
    succeed identity
        |. keyword open
        |. spaces
        |= lazy (\_ -> rhs)
        |. nextSymbol close


nextSymbol : String -> Parser ()
nextSymbol sym =
    delayedCommit spaces <|
        succeed identity
            |= keyword sym
            |. spaces


alternation : Parser Rhs
alternation =
    succeed Alternation
        |= lazy (\_ -> rhs)
        |= nextRhs "|"


concatenation : Parser Rhs
concatenation =
    succeed Concatenation
        |= lazy (\_ -> rhs)
        |= nextRhs ","


concat : Parser ( Rhs, Rhs )
concat =
    Parser.succeed (,)
        |= lazy (\_ -> rhs)
        |. spaces
        |. Parser.symbol ","
        |. spaces
        |= lazy (\_ -> rhs)


alter : Parser ( Rhs, Rhs )
alter =
    Parser.succeed (,)
        |= lazy (\_ -> rhs)
        |. spaces
        |. Parser.symbol "|"
        |. spaces
        |= lazy (\_ -> rhs)


nextRhs : String -> Parser Rhs
nextRhs sep =
    delayedCommit spaces <|
        succeed identity
            |. symbol sep
            |. spaces
            |= lazy (\_ -> rhs)


lazy : (() -> Parser a) -> Parser a
lazy delayedGenerator =
    Parser.succeed ()
        |> andThen delayedGenerator


nextBoundary : String -> Parser (a -> a)
nextBoundary boundary =
    delayedCommit (symbol boundary) <| succeed identity



--
-- comment =
--    succeed Comment
--          |. symbol "(*"
--          |. spaces
--          |= c
--          |. spaces
--          |. symbol "*)"
-- special =
--      succeed identity
--          |. symbol "?"
--          |. spaces
--          |= seq
--          |. spaces
--          |. symbol "?"
