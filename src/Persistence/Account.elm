module Persistence.Account exposing (Account, AccountStart, AccountV0, Accounts, account, codec, fromV0, v0Codec)

import Dict exposing (Dict)
import Persistence.YearMonth as YMC
import Serialize as S
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias Accounts =
    Dict Int Account


type alias Account =
    AccountV1


type alias AccountV1 =
    { id : Int
    , name : String
    , start : AccountStartV1
    }


type alias AccountV0 =
    { id : Int
    , name : String
    , start : AccountStartV0
    }


type alias AccountStart =
    AccountStartV1


type alias AccountStartV1 =
    { amount : Int, yearMonth : YearMonth }


type alias AccountStartV0 =
    { amount : Int, year : Int, month : Int }


account : Int -> String -> Int -> Int -> Int -> Account
account id n a y m =
    AccountV1 id n (AccountStartV1 a (YearMonth.new y m))


fromV0 : Dict Int AccountV0 -> Accounts
fromV0 =
    Dict.map (\_ a -> v0v1 a)


v0v1 : AccountV0 -> AccountV1
v0v1 a =
    AccountV1 a.id a.name (AccountStartV1 a.start.amount (YearMonth.new a.start.year a.start.month))



-- versioning-aware encoding


codec : S.Codec e Accounts
codec =
    S.dict S.int accountCodec


accountCodec : S.Codec e Account
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


type StorageVersions
    = V0 AccountV0
    | V1 AccountV1


v1Codec : S.Codec e AccountV1
v1Codec =
    S.record AccountV1
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .start accountStartV1Codec
        |> S.finishRecord


v0Codec : S.Codec e AccountV0
v0Codec =
    S.record AccountV0
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .start accountStartV0Codec
        |> S.finishRecord


accountStartV1Codec : S.Codec e AccountStartV1
accountStartV1Codec =
    S.record AccountStartV1
        |> S.field .amount S.int
        |> S.field .yearMonth YMC.codec
        |> S.finishRecord


accountStartV0Codec : S.Codec e AccountStartV0
accountStartV0Codec =
    S.record AccountStartV0
        |> S.field .amount S.int
        |> S.field .year S.int
        |> S.field .month S.int
        |> S.finishRecord
