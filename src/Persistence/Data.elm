module Persistence.Data exposing
    ( Account
    , AccountStart
    , Data
    , RawEntry
    , decode
    , empty
    , encode
    , rawEntry
    )

import Dict exposing (Dict)
import SHA1
import Serialize as S


type alias Data =
    DataV0


type alias DataV0 =
    { rawEntries : Dict String RawEntry
    , accounts : Dict Int Account
    , autoIncrement : Int
    }


type alias RawEntry =
    { id : String
    , line : String
    , accountId : Int
    }


type alias Account =
    { id : Int
    , name : String
    , start : AccountStart
    }


type alias AccountStart =
    { amount : Int, year : Int, month : Int }


rawEntry : Int -> String -> RawEntry
rawEntry accountId line =
    { id = sha1 line
    , line = line
    , accountId = accountId
    }


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex


empty : Data
empty =
    { rawEntries = Dict.empty
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
        |> S.field .rawEntries (S.dict S.string rawEntryCodec)
        |> S.field .accounts (S.dict S.int accountCodec)
        |> S.field .autoIncrement S.int
        |> S.finishRecord


rawEntryCodec =
    S.record RawEntry
        |> S.field .id S.string
        |> S.field .line S.string
        |> S.field .accountId S.int
        |> S.finishRecord


accountCodec : S.Codec e Account
accountCodec =
    S.record Account
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
