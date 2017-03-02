module Parsers exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Symbols exposing (..)
import Types exposing (..)
import Set


keySymbols : Set.Set String
keySymbols =
    Set.fromList [ "=", ",", ";", "|", "-" ]


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
        |. symbol "="
        |. spaces
        |= rhs
        |. spaces
        |. symbol ";"
        |. end


rhs : Parser Rhs
rhs =
    oneOf rhsOpts


rhsOpts : List (Parser Rhs)
rhsOpts =
    [ succeed Identifier |= identifier
      -- , succeed Terminal
      --     |= oneOf
      --         [ quotedString "\""
      --         , quotedString "'"
      --         ]
    , succeed Option |= enclosed "[" "]"
    , succeed Repetition |= enclosed "{" "}"
    , succeed Grouping |= enclosed "(" ")"
    , alternation
    ]


identifier : Parser String
identifier =
    variable isLetter isLetterOrDigitOrUnderscore noKeywords


alternation : Parser Rhs
alternation =
    succeed Alternation
        |. spaces
        |= lazy (\_ -> rhs)
        |. spaces
        |. symbol "|"
        |. spaces
        |= lazy (\_ -> rhs)
        |. spaces


character : Parser String
character =
    variable isCharacter isCharacter noKeywords


quotedString : String -> Parser String
quotedString boundary =
    succeed identity
        |. symbol boundary
        |= character
        |. symbol boundary


nextBoundary : String -> Parser (a -> a)
nextBoundary boundary =
    delayedCommit (symbol boundary) <| succeed identity


enclosed : String -> String -> Parser Rhs
enclosed open close =
    succeed identity
        |. symbol open
        |. spaces
        --|= oneOf rhsOpts
        |=
            oneOf [ succeed Identifier |= identifier ]
        |. symbol close


nextSymbol : String -> Parser ()
nextSymbol sym =
    delayedCommit spaces <|
        succeed identity
            |= symbol sym
            |. spaces



--|. nextSymbol close
--lazy (\_ -> rhs)
--
-- alter : Parser (a -> a)
-- alter =
--     succeed identity
--         |. lazy (\_ -> rhs)
--         |. alternation
--         |. lazy (\_ -> rhs)
--
--
--
-- conc : Parser (a -> a)
-- conc =
--     succeed identity
--         |. lazy (\_ -> rhs)
--         |. concatenation
--         |. lazy (\_ -> rhs)
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
