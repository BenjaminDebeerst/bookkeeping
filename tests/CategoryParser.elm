module CategoryParser exposing (..)

import Expect exposing (Expectation)
import Parser
import Processing.CategoryParser exposing (Categorization(..), categorizationParser)
import Test exposing (..)


parse_categorization : Test
parse_categorization =
    describe "Parse categorization strings"
        [ test "empty" <|
            \_ -> Parser.run categorizationParser "" |> Expect.equal (Ok Empty)
        , test "single" <|
            \_ -> Parser.run categorizationParser "CAT1" |> Expect.equal (Ok (One "CAT1"))
        , test "Trims whitespace" <|
            \_ -> Parser.run categorizationParser " CAT2 " |> Expect.equal (Ok (One "CAT2"))
        , test "split category" <|
            \_ -> Parser.run categorizationParser "CAT 100" |> Expect.equal (Ok (Multiple [ ( "CAT", 100 ) ]))
        , test "split categories" <|
            \_ -> Parser.run categorizationParser "CAT 100 CAT2 200" |> Expect.equal (Ok (Multiple [ ( "CAT", 100 ), ( "CAT2", 200 ) ]))
        ]
