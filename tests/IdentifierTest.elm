module IdentifierTest exposing (validIdentifierPass)

import BnfParsers exposing (Identifier(..), identifier)
import Test exposing (Test, test)
import TestHelpers exposing (expectEqual)



-- suite : Test
-- suite =
--     describe "identifier tests"
--         [ validIdentifierPass
--         ]


validIdentifierPass : Test
validIdentifierPass =
    test "valid identifier passes" <|
        \() -> expectEqual identifier "qwert" <| Identifier "qwert"
