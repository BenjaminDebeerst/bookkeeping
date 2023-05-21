module CategoryParser exposing (..)

import Expect exposing (Expectation)
import Processing.CategoryParser exposing (Categorization(..), parseCategorization)
import Test exposing (..)


parse_categorization : Test
parse_categorization =
    describe "Parse categorization strings"
        [ test "empty" <|
            \_ -> parseCategorization "" |> Expect.equal (Ok Empty)
        , test "single" <|
            \_ -> parseCategorization "CAT1" |> Expect.equal (Ok (One "CAT1"))
        , test "Trims whitespace" <|
            \_ -> parseCategorization " CAT2 " |> Expect.equal (Ok (One "CAT2"))
        , test "split category" <|
            \_ -> parseCategorization "CAT 100" |> Expect.equal (Ok (Multiple [ ( "CAT", 100 ) ]))
        , test "split categories" <|
            \_ -> parseCategorization "CAT 100 CAT2 200" |> Expect.equal (Ok (Multiple [ ( "CAT", 100 ), ( "CAT2", 200 ) ]))
        ]
