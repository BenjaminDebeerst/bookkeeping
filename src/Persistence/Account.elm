module Persistence.Account exposing (..)

import Dict exposing (Dict)
import Serialize as S


type alias Accounts =
    Dict Int Account


type alias Account =
    { id : Int
    , name : String
    , start : AccountStart
    }


type alias AccountStart =
    { amount : Int, year : Int, month : Int }



-- Codecs


codec : S.Codec String Accounts
codec =
    S.dict S.int accountCodec


accountCodec : S.Codec String Account
accountCodec =
    S.record Account
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .start accountStartCodec
        |> S.finishRecord


accountStartCodec : S.Codec String AccountStart
accountStartCodec =
    S.record AccountStart
        |> S.field .amount S.int
        |> S.field .year S.int
        |> S.field .month S.int
        |> S.finishRecord
