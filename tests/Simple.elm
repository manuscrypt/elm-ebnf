module Simple exposing (..)

import Test exposing (..)
import Expect


suite : Test
suite =
    describe "The simple module"
        [ describe "aabb parser"
            [ test "can parse a" <|
                \() -> Expect.equal "a" "a"
            ]
        ]
