module TestHelpers exposing (expectEqual, expectFail)

import Expect
import Parser exposing (..)


expectFail : Parser a -> String -> Expect.Expectation
expectFail parseFn input =
    case Parser.run parseFn input of
        Ok res ->
            Expect.fail <| "should not be able to parse " ++ input

        Err err ->
            Expect.pass


expectEqual : Parser a -> String -> a -> Expect.Expectation
expectEqual parseFn input expected =
    case Parser.run parseFn input of
        Ok res ->
            Expect.equal res expected

        Err err ->
            Expect.fail <| Debug.toString err
