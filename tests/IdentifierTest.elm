module IdentifierTest exposing (..)

import Parsers exposing (..)
import Test exposing (..)
import TestHelpers exposing (expectEqual, expectFail)


suite : Test
suite =
    describe "identifier tests"
        [ validIdentifierPass
        ]


validIdentifierPass : Test
validIdentifierPass =
    test "valid identifier passes" <|
        \() -> expectEqual identifier "qwert" <| Identifier "qwert"
