module ExpressionTest exposing (..)

import Parsers exposing (..)
import Test exposing (..)
import TestHelpers exposing (expectEqual, expectFail)


suite : Test
suite =
    describe "repetition tests"
        [ validRepetitionPass
        , validOptionPass
        , validOptionsPass
        , validGroupPass
        , validAlternationPass
        , validConcatenationPass
        ]


validRepetitionPass : Test
validRepetitionPass =
    test "valid repetition passes" <|
        \() -> expectEqual rep "{ qwert }" <| Repetition [ RefId (Identifier "qwert") ]


validOptionPass : Test
validOptionPass =
    test "valid option passes" <|
        \() -> expectEqual opt "[ qwert ]" <| Option [ RefId (Identifier "qwert") ]


validOptionsPass : Test
validOptionsPass =
    test "valid options (multiple) passes" <|
        \() -> expectEqual opt "[ qwert, bert ]" <| Option [ RefId (Identifier "qwert"), RefId (Identifier "bert") ]


validGroupPass : Test
validGroupPass =
    test "valid group passes" <|
        \() -> expectEqual grp "( qwert )" <| Group [ RefId (Identifier "qwert") ]


validAlternationPass : Test
validAlternationPass =
    test "valid alternation passes" <|
        \() -> expectEqual alternation "'A' | 'B'" <| Alternation (Literal "A") (Literal "B")


validConcatenationPass : Test
validConcatenationPass =
    test "valid concatenation passes" <|
        \() -> expectEqual concatenation "'A' , 'B'" <| Concatenation (Literal "A") (Literal "B")
