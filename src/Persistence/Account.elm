module Persistence.Account exposing (Account, AccountStart, AccountV0, Accounts, account, codec, fromV0, v0Codec)

import Dict exposing (Dict)
import Serialize as S


type alias Accounts =
    Dict Int Account


type alias Account =
    AccountV0


type alias AccountV0 =
    { id : Int
    , name : String
    , start : AccountStart
    }


type alias AccountStart =
    { amount : Int, year : Int, month : Int }


account : String -> Int -> Int -> Int -> Account
account n a y m =
    AccountV0 0 n (AccountStart a y m)


fromV0 : Dict Int AccountV0 -> Accounts
fromV0 dict =
    dict



-- versioning-aware encoding


codec : S.Codec e Accounts
codec =
    S.dict S.int accountCodec


accountCodec : S.Codec e Account
accountCodec =
    S.customType
        (\v0Encoder value ->
            case value of
                V0 record ->
                    v0Encoder record
        )
        |> S.variant1 V0 v0Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        storage
            )
            V0


type StorageVersions
    = V0 AccountV0


v0Codec : S.Codec e AccountV0
v0Codec =
    S.record AccountV0
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .start accountStartCodec
        |> S.finishRecord


accountStartCodec : S.Codec e AccountStart
accountStartCodec =
    S.record AccountStart
        |> S.field .amount S.int
        |> S.field .year S.int
        |> S.field .month S.int
        |> S.finishRecord
