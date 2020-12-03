module WsnParsers exposing (Expression, Factor(..), Identifier, Production, Syntax(..), Term, expression, factor, factors, group, id, identifier, literal, option, production, productionsHelp, repetition, syntax, term)

--import Parser.Advanced as Parser exposing (..)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), end, loop, map, oneOf, sequence, spaces, succeed, symbol, variable)
import Set
import Symbols exposing (isCharacter, isLetter, isLetterOrDigitOrUnderscore)


type Syntax
    = Syntax (List Production)


type alias Production =
    { identifier : String
    , expression : Expression
    }


type alias Expression =
    List Term


type alias Term =
    List Factor


type alias Identifier =
    String


type Factor
    = Id String
    | Literal String
    | Repetition Expression
    | Option Expression
    | Group Expression


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
            |. spaces
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
        |= expression
        |. spaces
        |. symbol "."


expression : Parser Expression
expression =
    Parser.sequence
        { start = ""
        , separator = "|"
        , end = ""
        , spaces = spaces
        , item = term
        , trailing = Forbidden -- demand a trailing semi-colon
        }



-- expression : Parser Expression
-- expression =
--     loop [] terms
-- terms : List Term -> Parser (Step (List Term) (List Term))
-- terms revTerms =
--     oneOf
--         [ succeed (\t -> Loop (t :: revTerms))
--             |= term
--             |. symbol "|"
--         , succeed (\t -> Loop (t :: revTerms))
--             |= term
--         , succeed ()
--             |> map (\_ -> Done (List.reverse revTerms))
--         ]


term : Parser Term
term =
    loop [] factors


factors : List Factor -> Parser (Step (List Factor) (List Factor))
factors revFactors =
    oneOf
        [ succeed (\f -> Loop (f :: revFactors))
            |= factor
        , succeed ()
            |> map (\_ -> Done (List.reverse revFactors))
        ]


factor : Parser Factor
factor =
    oneOf [ id, literal, repetition, option, group ]


repetition : Parser Factor
repetition =
    succeed Repetition
        |. spaces
        |. symbol "{"
        |. spaces
        |= expression
        |. spaces
        |. symbol "}"
        |. spaces


option : Parser Factor
option =
    succeed Option
        |. spaces
        |. symbol "["
        |. spaces
        |= expression
        |. spaces
        |. symbol "]"
        |. spaces


group : Parser Factor
group =
    succeed Group
        |. spaces
        |. symbol "("
        |. spaces
        |= expression
        |. spaces
        |. symbol ")"
        |. spaces


id : Parser Factor
id =
    succeed Id |= identifier


identifier : Parser Identifier
identifier =
    succeed identity
        |. spaces
        |= variable { start = isLetter, inner = isLetterOrDigitOrUnderscore, reserved = Set.empty }
        |. spaces


literal : Parser Factor
literal =
    succeed Literal
        |. spaces
        |. symbol "\""
        |. spaces
        |= variable { start = isCharacter, inner = isCharacter, reserved = Set.empty }
        |. spaces
        |. symbol "\""
        |. spaces
