module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, pair, string)
import Storage exposing (decode, encode)
import Test exposing (..)


suite : Test
suite =
    describe "Data serialization"
        [ fuzz (list (pair string string)) "encode and decode are inverse" <|
            \data -> { bookEntries = data } |> encode |> decode |> Expect.equal { bookEntries = data }
        , fuzz (list (pair string string)) "spaces are trimmed before decoding" <|
            \data -> { bookEntries = data } |> encode |> (\s -> " " ++ s ++ " ") |> decode |> Expect.equal { bookEntries = data }
        ]
