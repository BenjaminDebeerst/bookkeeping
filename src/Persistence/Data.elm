module Persistence.Data exposing
    ( Data
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
    }


type alias Entry =
    { id : String
    , date : Date
    , description : String
    , amount : Int
    }


empty : Data
empty =
    { bookEntries = Dict.empty }


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
        |> S.finishRecord


dateCodec : S.Codec e Date
dateCodec =
    S.customType
        (\encoder d -> encoder (Date.year d) (Date.month d) (Date.day d))
        |> S.variant3 Date.date S.int S.int S.int
        |> S.finishCustomType
