module FlagsDecoder exposing (..)

import Expect
import Json.Decode
import Persistence.Data as Data
import Shared
import Test exposing (Test, describe, test)


emptyDBString =
    "\"[1,[4,[[1,[0,[]]],[],[],[],[0,[]],[0,[0,0,[]]],0]]]\""


format_euro_str : Test
format_euro_str =
    describe "Format Cent values to euros"
        [ test "handles the empty string" <|
            \_ -> Json.Decode.decodeString Shared.decoder "\"\"" |> Expect.equal (Ok Nothing)
        , test "imports data" <|
            \_ -> Json.Decode.decodeString Shared.decoder emptyDBString |> Expect.equal (Ok <| Just Data.empty)
        ]
