module Main exposing (..)

import ExpressionTest exposing (..)
import IdentifierTest exposing (..)
import QuotedStringsTest exposing (..)
import SymbolsTest exposing (..)
import Test exposing (..)
import Test.Runner.Html


suite : Test
suite =
    describe "The EBNF parser module"
        [ SymbolsTest.suite
        , QuotedStringsTest.suite
        , IdentifierTest.suite
        , ExpressionTest.suite
        ]


main : Test.Runner.Html.TestProgram
main =
    suite
        |> Test.Runner.Html.run
