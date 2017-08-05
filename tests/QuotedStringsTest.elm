module QuotedStringsTest exposing (..)

import Parsers exposing (..)
import Test exposing (..)
import TestHelpers exposing (expectEqual, expectFail)


suite : Test
suite =
    describe "quoted strings tests"
        [ singleQuotedStringValidLettersPass
        , doubleQuotedStringValidLettersPass
        , doubleQuoteInSingleQuotesPass
        , singleQuoteInDoubleQuotesPass
        ]


singleQuotedStringValidLettersPass : Test
singleQuotedStringValidLettersPass =
    test "valid single quoted string with valid letters passes" <|
        \() -> expectEqual sLiteral "'qwert'" <| "qwert"


doubleQuotedStringValidLettersPass : Test
doubleQuotedStringValidLettersPass =
    test "valid double quoted string with valid letters passes" <|
        \() -> expectEqual dLiteral (toString "qwert") <| "qwert"


doubleQuoteInSingleQuotesPass : Test
doubleQuoteInSingleQuotesPass =
    test "a double quote within single quotes passes" <|
        \() -> expectEqual dLiteral "'\"'" <| "\""


singleQuoteInDoubleQuotesPass : Test
singleQuoteInDoubleQuotesPass =
    test "a single quote within double quotes passes" <|
        \() -> expectEqual dLiteral (toString "'") <| "'"
