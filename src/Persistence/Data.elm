module Persistence.Data exposing
    ( Account
    , AccountStart
    , Data
    , RawAccountEntry
    , RawEntry
    , decode
    , empty
    , encode
    , rawAccountEntry
    , rawEntry
    )

import Dict exposing (Dict)
import SHA1
import Serialize as S


type alias Data =
    DataV0


type alias DataV0 =
    { rawEntries : Dict String RawAccountEntry
    , accounts : Dict Int Account
    , autoIncrement : Int
    }


type alias RawEntry =
    { id : String
    , line : String
    }


rawEntry : String -> RawEntry
rawEntry line =
    { id = sha1 line
    , line = line
    }


type alias RawAccountEntry =
    { entry : RawEntry
    , account : Int
    }


rawAccountEntry : Account -> String -> RawAccountEntry
rawAccountEntry account line =
    { entry = rawEntry line
    , account = account.id
    }


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex


type alias Account =
    { id : Int
    , name : String
    , start : AccountStart
    }


type alias AccountStart =
    { amount : Int, year : Int, month : Int }


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
        |> S.field .rawEntries
            (S.dict S.string
                (S.record RawAccountEntry
                    |> S.field .entry
                        (S.record RawEntry
                            |> S.field .id S.string
                            |> S.field .line S.string
                            |> S.finishRecord
                        )
                    |> S.field .account S.int
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
