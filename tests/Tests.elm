module Tests exposing (..)

import Test exposing (..)
import Expect
import Parsers exposing (..)
import Parser
import Types exposing (..)


suite : Test
suite =
    describe "The ebnf module"
        [ describe "terminal parser"
            -- Nest as many descriptions as you like.
            [ test "can parse double quoted string" <|
                \() ->
                    case Parser.run Parsers.doubleQuotedString "\"astring\"" of
                        Ok res ->
                            Expect.equal "\"astring\"" res

                        Err err ->
                            Expect.fail <| toString err
            , test "can parse single quoted string" <|
                \() ->
                    case Parser.run Parsers.singleQuotedString "'astring'" of
                        Ok res ->
                            Expect.equal "'astring'" res

                        Err err ->
                            Expect.fail <| toString err
            , test "double quoted can NOT contain single quoted (or any other non-character)" <|
                \() ->
                    case Parser.run Parsers.singleQuotedString "\"ast'ring\"" of
                        Ok res ->
                            Expect.fail "single quote in double quoted string should fail"

                        Err err ->
                            Expect.equal True True
            , test "can parse Terminal" <|
                \() ->
                    case Parser.run Parsers.terminal "\"astring\"" of
                        Ok res ->
                            case res of
                                Terminal t ->
                                    Expect.equal "\"astring\"" t

                                _ ->
                                    Expect.fail <| "expected a terminal"

                        Err err ->
                            Expect.fail <| toString err
            , test "can parse Identifier" <|
                \() ->
                    case Parser.run Parsers.ident "hello" of
                        Ok res ->
                            case res of
                                Identifier t ->
                                    Expect.equal "hello" t

                                _ ->
                                    Expect.fail <| "expected a terminal"

                        Err err ->
                            Expect.fail <| toString err
            , test "identifier cannot start with number" <|
                \() ->
                    case Parser.run Parsers.ident "4hello" of
                        Ok res ->
                            Expect.fail "not an identifier: 4hello"

                        Err err ->
                            Expect.equal True True
            , test "can parse Optional Identifier" <|
                \() ->
                    case Parser.run Parsers.option "[hello]" of
                        Ok res ->
                            case res of
                                Option t ->
                                    case t of
                                        Identifier i ->
                                            Expect.equal "hello" i

                                        _ ->
                                            Expect.fail "expected an identifier"

                                _ ->
                                    Expect.fail <| "expected an Option"

                        Err err ->
                            Expect.fail <| toString err
            , test "can parse Alternation" <|
                \() ->
                    case Parser.run Parsers.alternation "a | b" of
                        Ok res ->
                            Expect.equal res (Alternate (Identifier "a") (Single (Identifier "b")))

                        Err err ->
                            Expect.fail <| toString err
            , test "can parse Alternation 2" <|
                \() ->
                    case Parser.run Parsers.alternation "a | b | c" of
                        Ok res ->
                            Expect.equal (Debug.log "res" res) (Alternate (Identifier "a") (Alternate (Identifier "B") (Single (Identifier "b"))))

                        Err err ->
                            Expect.fail <| toString err
            , test "can parse Concatenation" <|
                \() ->
                    case Parser.run Parsers.concatenation "a , b" of
                        Ok res ->
                            Expect.equal res (Concatenation (Identifier "a") (Identifier "b"))

                        Err err ->
                            Expect.fail <| toString err
            ]
        ]
