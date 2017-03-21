module SymbolsTest exposing (..)

import Test exposing (..)
import Parsers exposing (..)
import TestHelpers exposing (expectFail, expectEqual)


suite : Test
suite =
    describe "terminal Tests"
        [ singleValidLetterSuccess
        , singleInvalidLetterFailure
        , multiValidLetterSuccess
        , multiLettersOneInvalidFailure
        ]


singleValidLetterSuccess : Test
singleValidLetterSuccess =
    test "valid single letter passes" <|
        \() -> expectEqual anyString "q" <| "q"


singleInvalidLetterFailure : Test
singleInvalidLetterFailure =
    test "invalid single letter fails" <|
        \() -> expectFail anyString "ä"


multiValidLetterSuccess : Test
multiValidLetterSuccess =
    test "valid letters passes" <|
        \() -> expectEqual anyString "qwErt" <| "qwErt"


multiLettersOneInvalidFailure : Test
multiLettersOneInvalidFailure =
    test "invalid letter within multiple stops parsing there" <|
        \() -> expectEqual anyString "gräms" <| "gr"
