module Persistence.Account exposing (Account, AccountStart, AccountV0, Accounts, account, codec, fromV0, v0Codec)

import Dict exposing (Dict)
import Serialize as S


type alias Accounts =
    Dict Int Account


type alias Account =
    AccountV1


fromV0 : Dict Int AccountV0 -> Accounts
fromV0 dict =
    Dict.map (\_ -> v0v1) dict


type alias AccountV1 =
    { id : Int
    , name : String
    , start : AccountStart
    , comment : String
    }


type alias AccountV0 =
    { id : Int
    , name : String
    , start : AccountStart
    }


type alias AccountStart =
    { amount : Int, year : Int, month : Int }


account : String -> Int -> Int -> Int -> String -> Account
account n a y m c =
    AccountV1 0 n (AccountStart a y m) c



-- Codecs


codec : S.Codec String Accounts
codec =
    S.dict S.int accountCodec


v1Codec : S.Codec String AccountV1
v1Codec =
    S.record AccountV1
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .start accountStartCodec
        |> S.field .comment S.string
        |> S.finishRecord


v0Codec : S.Codec String AccountV0
v0Codec =
    S.record AccountV0
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



-- versioning-aware encoding


type StorageVersions
    = V0 AccountV0
    | V1 AccountV1


accountCodec : S.Codec String Account
accountCodec =
    S.customType
        (\v0Encoder v1Encoder value ->
            case value of
                V0 record ->
                    v0Encoder record

                V1 record ->
                    v1Encoder record
        )
        |> S.variant1 V0 v0Codec
        |> S.variant1 V1 v1Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        v0v1 storage

                    V1 storage ->
                        storage
            )
            V1


v0v1 : AccountV0 -> AccountV1
v0v1 v0 =
    AccountV1 v0.id v0.name v0.start ""
