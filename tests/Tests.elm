module Tests exposing (..)

import Test exposing (..)
import SymbolsTest exposing (..)
import QuotedStringsTest exposing (..)
import IdentifierTest exposing (..)
import ExpressionTest exposing (..)


suite : Test
suite =
    describe "The EBNF parser module"
        [ SymbolsTest.suite
        , QuotedStringsTest.suite
        , IdentifierTest.suite
        , ExpressionTest.suite
        ]
