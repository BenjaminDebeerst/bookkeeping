module FormatEuro exposing (..)

import Components.Layout exposing (formatEuroStr)
import Expect exposing (Expectation)
import Test exposing (..)


format_euro_str : Test
format_euro_str =
    describe "Format Cent values to euros"
        [ test "zero cents are formatted correctly" <|
            \_ -> formatEuroStr 0 |> Expect.equal "0.00 €"
        , test "cent values below 10 are formatted correctly" <|
            \_ -> formatEuroStr 4 |> Expect.equal "0.04 €"
        , test "negative cent values below 10 are formatted correctly" <|
            \_ -> formatEuroStr -4 |> Expect.equal "-0.04 €"
        , test "cent values above 10 are formatted correctly" <|
            \_ -> formatEuroStr 23 |> Expect.equal "0.23 €"
        , test "negative cent values above 10 are formatted correctly" <|
            \_ -> formatEuroStr -23 |> Expect.equal "-0.23 €"
        , test "cent values above 100 are formatted correctly" <|
            \_ -> formatEuroStr 4223 |> Expect.equal "42.23 €"
        , test "negative cent values above 100 are formatted correctly" <|
            \_ -> formatEuroStr -223 |> Expect.equal "-2.23 €"
        ]
