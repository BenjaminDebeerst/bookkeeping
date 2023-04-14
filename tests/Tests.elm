module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string)
import Test exposing (..)
import Main exposing (encode, decode)

suite : Test
suite =
  describe "Data serialization"
    [ fuzz (list (list string)) "encode and decode are inverse" <|
        \data -> data |> encode |> decode |> Expect.equal data
    , fuzz (list (list string)) "spaces are trimmed before decoding" <|
        \data -> data |> encode |> (\s -> " " ++ s ++ " ") |> decode |> Expect.equal data
    ]
