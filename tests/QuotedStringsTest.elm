module QuotedStringsTest exposing (doubleQuoteInSingleQuotesPass, doubleQuotedStringValidLettersPass, singleQuoteInDoubleQuotesPass, singleQuotedStringValidLettersPass)

import Parsers exposing (..)
import Test exposing (..)
import TestHelpers exposing (expectEqual, expectFail)


singleQuotedStringValidLettersPass : Test
singleQuotedStringValidLettersPass =
    test "valid single quoted string with valid letters passes" <|
        \() -> expectEqual sLiteral "'qwert'" <| "qwert"


doubleQuotedStringValidLettersPass : Test
doubleQuotedStringValidLettersPass =
    test "valid double quoted string with valid letters passes" <|
        \() -> expectEqual dLiteral (Debug.toString "qwert") <| "qwert"


doubleQuoteInSingleQuotesPass : Test
doubleQuoteInSingleQuotesPass =
    test "a double quote within single quotes passes" <|
        \() -> expectEqual sLiteral "'\"'" <| "\""


singleQuoteInDoubleQuotesPass : Test
singleQuoteInDoubleQuotesPass =
    test "a single quote within double quotes passes" <|
        \() -> expectEqual dLiteral (Debug.toString "'") <| "'"
