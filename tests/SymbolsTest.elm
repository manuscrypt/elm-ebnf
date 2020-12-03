module SymbolsTest exposing (multiLettersOneInvalidFailure, multiValidLetterSuccess, singleInvalidLetterFailure, singleValidLetterSuccess)

import BnfParsers exposing (anyString)
import Test exposing (Test, test)
import TestHelpers exposing (expectEqual, expectFail)



-- suite : Test
-- suite =
--     describe "terminal Tests"
--         [ singleValidLetterSuccess
--         , singleInvalidLetterFailure
--         , multiValidLetterSuccess
--         , multiLettersOneInvalidFailure
--         ]


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
