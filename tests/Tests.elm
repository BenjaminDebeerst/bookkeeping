module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, pair, string)
import Persistence.Data exposing (decode, encode)
import Test exposing (..)
import Layout exposing (formatEuroStr)

{-
suite : Test
suite =
    describe "Data serialization"
        [ fuzz (list (pair string string)) "encode and decode are inverse" <|
            \data -> { bookEntries = data } |> encode |> decode |> Expect.equal { bookEntries = data }
        , fuzz (list (pair string string)) "spaces are trimmed before decoding" <|
            \data -> { bookEntries = data } |> encode |> (\s -> " " ++ s ++ " ") |> decode |> Expect.equal { bookEntries = data }
        ]
-}

format_euro_str : Test
format_euro_str =
    describe "Format Cent values to euros"
        [ test "zero cents are formatted correctly" <|
             \_ -> (formatEuroStr 0) |> Expect.equal "0.00 €"
        , test "cent values below 10 are formatted correctly" <|
             \_ -> (formatEuroStr 4) |> Expect.equal "0.04 €"
        , test "negative cent values below 10 are formatted correctly" <|
             \_ -> (formatEuroStr -4) |> Expect.equal "-0.04 €"
        , test "cent values above 10 are formatted correctly" <|
             \_ -> (formatEuroStr 23) |> Expect.equal "0.23 €"
        , test "negative cent values above 10 are formatted correctly" <|
             \_ -> (formatEuroStr -23) |> Expect.equal "-0.23 €"
        , test "cent values above 100 are formatted correctly" <|
             \_ -> (formatEuroStr 4223) |> Expect.equal "42.23 €"
        , test "negative cent values above 100 are formatted correctly" <|
             \_ -> (formatEuroStr -223) |> Expect.equal "-2.23 €"
         ]
