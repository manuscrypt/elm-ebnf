module ExpressionTest exposing (..)

import Test exposing (..)
import Parsers exposing (..)
import TestHelpers exposing (expectFail, expectEqual)


suite : Test
suite =
    describe "repetition tests"
        [ validRepetitionPass
        , validOptionPass
        , validGroupPass
        , validAlternationPass
        ]


validRepetitionPass : Test
validRepetitionPass =
    test "valid repetition passes" <|
        \() -> expectEqual rep "{ qwert }" <| Repetition ([ Identifier "qwert" ])


validOptionPass : Test
validOptionPass =
    test "valid option passes" <|
        \() -> expectEqual opt "[ qwert ]" <| Option ([ Identifier "qwert" ])


validGroupPass : Test
validGroupPass =
    test "valid group passes" <|
        \() -> expectEqual grp "( qwert )" <| Group ([ Identifier "qwert" ])


validAlternationPass : Test
validAlternationPass =
    test "valid alternation passes" <|
        \() -> expectEqual alternation "'A' | 'B'" <| [ Literal "A", Literal "B" ]
