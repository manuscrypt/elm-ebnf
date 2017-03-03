module Parsers exposing (..)

import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Symbols exposing (..)
import Types exposing (..)
import Set


keySymbols : Set.Set String
keySymbols =
    Set.fromList [ "=", ",", ";", "|", "-" ]


doubleQuotedString : Parser String
doubleQuotedString =
    variable (\c -> c == '"') (\c -> isCharacter c) (Set.fromList [])


singleQuotedString : Parser String
singleQuotedString =
    variable (\c -> c == '\'') (\c -> isCharacter c) (Set.fromList [])


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
        |= rhs
        |. endOfRule


endOfRule : Parser (a -> a)
endOfRule =
    delayedCommit spaces <|
        succeed identity
            |. keyword ";"


rhsNonLazy : List (Parser Rhs)
rhsNonLazy =
    [ --alternation
      --, concatenation
      option
    , grouping
    , repetition
    , terminal
    , ident
    ]


rhs : Parser Rhs
rhs =
    delayedCommit spaces <|
        succeed identity
            |= lazy (\_ -> oneOf rhsNonLazy)



-- [ --lazy (\_ -> alternation)
--   --, lazy (\_ -> concatenation)
--   lazy (\_ -> option)
-- , lazy (\_ -> grouping)
-- , lazy (\_ -> repetition)
-- , terminal
-- , ident
-- ]


concatenation : Parser Rhs
concatenation =
    succeed Concatenation
        |= rhs
        |= con


alternation : Parser Alternation
alternation =
    oneOf
        [ succeed Alternate |= rhs |= lazy (\_ -> alternation)
        , succeed Single |= rhs
        ]



-- succeed Alternation
--     |= andThen (\n -> rhsListHelp alt [ n ]) rhs


rhsListHelp : Parser Rhs -> List Rhs -> Parser (List Rhs)
rhsListHelp next revs =
    oneOf
        [ next
            |> andThen (\n -> rhsListHelp next (n :: revs))
        , succeed (List.reverse revs)
        ]


alt : Parser Rhs
alt =
    delayedCommit spaces <|
        succeed identity
            |. symbol "|"
            |. spaces
            |= rhs


con : Parser Rhs
con =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= rhs


option : Parser Rhs
option =
    succeed Option
        |. spaces
        |. keyword "["
        |. spaces
        |= rhs
        |. spaces
        |. keyword "]"
        |. spaces


grouping : Parser Rhs
grouping =
    succeed Grouping
        |. spaces
        |. keyword "("
        |. spaces
        |= rhs
        |. spaces
        |. keyword ")"
        |. spaces


repetition : Parser Rhs
repetition =
    succeed Repetition
        |. spaces
        |. keyword "{"
        |. spaces
        |= rhs
        |. spaces
        |. keyword "}"
        |. spaces


ident : Parser Rhs
ident =
    succeed Identifier |= identifier



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
