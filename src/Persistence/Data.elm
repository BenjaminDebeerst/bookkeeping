module Persistence.Data exposing
    ( Data
    , decode
    , empty
    , encode
    )

import Dict exposing (Dict)
import Persistence.Account exposing (Account, accountCodec)
import Persistence.Category exposing (Category, categoryCodec)
import Persistence.ImportProfile exposing (ImportProfile, profileCodec)
import Persistence.RawEntry exposing (RawEntry, rawEntryCodec)
import Serialize as S exposing (Error)


type alias Data =
    DataV0


type alias DataV0 =
    { rawEntries : Dict String RawEntry
    , accounts : Dict Int Account
    , categories : Dict Int Category
    , importProfiles : Dict Int ImportProfile
    , autoIncrement : Int
    }


empty : Data
empty =
    { rawEntries = Dict.empty
    , accounts = Dict.empty
    , categories = Dict.empty
    , importProfiles = Dict.empty
    , autoIncrement = 0
    }


encode : Data -> String
encode storage =
    S.encodeToString dataCodec storage


decode : String -> Result (Error String) Data
decode value =
    case value of
        "" ->
            Ok empty

        s ->
            S.decodeFromString dataCodec (String.trim s)



-- versioning-aware encoding


type StorageVersions
    = V0 DataV0


dataCodec : S.Codec String Data
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


v0Codec : S.Codec String Data
v0Codec =
    S.record DataV0
        |> S.field .rawEntries (S.dict S.string rawEntryCodec)
        |> S.field .accounts (S.dict S.int accountCodec)
        |> S.field .categories (S.dict S.int categoryCodec)
        |> S.field .importProfiles (S.dict S.int profileCodec)
        |> S.field .autoIncrement S.int
        |> S.finishRecord
