module QuotedStringsTest exposing (..)

import Test exposing (..)
import Parsers exposing (..)
import TestHelpers exposing (expectFail, expectEqual)


suite : Test
suite =
    describe "quoted strings tests"
        [ singleQuotedStringValidLettersPass
        , doubleQuotedStringValidLettersPass
        ]


singleQuotedStringValidLettersPass : Test
singleQuotedStringValidLettersPass =
    test "valid single quoted string with valid letters passes" <|
        \() -> expectEqual sLiteral "'qwert'" <| "qwert"


doubleQuotedStringValidLettersPass : Test
doubleQuotedStringValidLettersPass =
    test "valid double quoted string with valid letters passes" <|
        \() -> expectEqual dLiteral (toString "qwert") <| "qwert"
