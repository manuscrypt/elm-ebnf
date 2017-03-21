module IdentifierTest exposing (..)

import Test exposing (..)
import Parsers exposing (..)
import TestHelpers exposing (expectFail, expectEqual)


suite : Test
suite =
    describe "identifier tests"
        [ validIdentifierPass
        ]


validIdentifierPass : Test
validIdentifierPass =
    test "valid identifier passes" <|
        \() -> expectEqual identifier "qwert" <| Identifier "qwert"
