module Persistence.Data exposing
    ( Account
    , AccountStart
    , Data
    , Entry
    , decode
    , empty
    , encode
    )

import Dict exposing (Dict)
import Serialize as S
import Time.Date as Date exposing (Date)


type alias Data =
    DataV0


type alias DataV0 =
    { bookEntries : Dict String Entry
    , accounts : Dict Int Account
    , autoIncrement : Int
    }


type alias Entry =
    { id : String
    , date : Date
    , description : String
    , amount : Int
    }


type alias Account =
    { id : Int
    , name : String
    , start : AccountStart
    }


type alias AccountStart =
    { amount : Int, year : Int, month : Int }


empty : Data
empty =
    { bookEntries = Dict.empty
    , accounts = Dict.empty
    , autoIncrement = 0
    }


encode : Data -> String
encode storage =
    S.encodeToString dataCodec storage


decode : String -> Data
decode value =
    case S.decodeFromString dataCodec (String.trim value) of
        Ok data ->
            data

        Err _ ->
            empty



-- versioning-aware encoding


type StorageVersions
    = V0 DataV0


dataCodec : S.Codec e Data
dataCodec =
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


v0Codec : S.Codec e Data
v0Codec =
    S.record DataV0
        |> S.field .bookEntries
            (S.dict S.string
                (S.record Entry
                    |> S.field .id S.string
                    |> S.field .date dateCodec
                    |> S.field .description S.string
                    |> S.field .amount S.int
                    |> S.finishRecord
                )
            )
        |> S.field .accounts
            (S.dict S.int
                (S.record Account
                    |> S.field .id S.int
                    |> S.field .name S.string
                    |> S.field .start accountStartCodec
                    |> S.finishRecord
                )
            )
        |> S.field .autoIncrement S.int
        |> S.finishRecord


accountStartCodec : S.Codec e AccountStart
accountStartCodec =
    S.record AccountStart
        |> S.field .amount S.int
        |> S.field .year S.int
        |> S.field .month S.int
        |> S.finishRecord


dateCodec : S.Codec e Date
dateCodec =
    S.customType
        (\encoder d -> encoder (Date.year d) (Date.month d) (Date.day d))
        |> S.variant3 Date.date S.int S.int S.int
        |> S.finishCustomType
