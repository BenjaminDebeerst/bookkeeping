module Persistence.Data exposing
    ( Data
    , empty
    , jsonDecoder
    , jsonEncoder
    )

import Dict exposing (Dict)
import Json.Decode
import Json.Encode exposing (Value)
import Persistence.Account as Account exposing (AccountV0, Accounts)
import Persistence.AggregationGroups as AggreationGroup exposing (AggregationGroups)
import Persistence.Audits as Audits exposing (Audits)
import Persistence.Category as Category exposing (Categories, CategoryV0)
import Persistence.ImportProfile as ImportProfile exposing (ImportProfileV0, ImportProfiles)
import Persistence.RawEntry as RawEntry exposing (RawEntries, RawEntryV0)
import Serialize as S exposing (Error)


type alias Data =
    DataV4


type alias DataV4 =
    { rawEntries : RawEntries
    , accounts : Accounts
    , categories : Categories
    , importProfiles : ImportProfiles
    , audits : Audits
    , aggregationGroups : AggregationGroups
    , autoIncrement : Int
    }


type alias DataV3 =
    { rawEntries : RawEntries
    , accounts : Accounts
    , categories : Categories
    , importProfiles : ImportProfiles
    , audits : Audits
    , autoIncrement : Int
    }


type alias DataV2 =
    { rawEntries : RawEntries
    , accounts : Accounts
    , categories : Categories
    , importProfiles : ImportProfiles
    , autoIncrement : Int
    }


type alias DataV1 =
    { rawEntries : Dict String RawEntryV0
    , accounts : Accounts
    , categories : Categories
    , importProfiles : ImportProfiles
    , autoIncrement : Int
    }


type alias DataV0 =
    { rawEntries : Dict String RawEntryV0
    , accounts : Dict Int AccountV0
    , categories : Dict Int CategoryV0
    , importProfiles : Dict Int ImportProfileV0
    , autoIncrement : Int
    }


v3v4 : DataV3 -> DataV4
v3v4 v3 =
    DataV4
        v3.rawEntries
        v3.accounts
        v3.categories
        v3.importProfiles
        v3.audits
        AggreationGroup.empty
        v3.autoIncrement


v2v3 : DataV2 -> DataV3
v2v3 v2 =
    DataV3
        v2.rawEntries
        v2.accounts
        v2.categories
        v2.importProfiles
        Audits.empty
        v2.autoIncrement


v1v2 : DataV1 -> DataV2
v1v2 v1 =
    DataV2
        (RawEntry.fromV0 v1.rawEntries)
        v1.accounts
        v1.categories
        v1.importProfiles
        v1.autoIncrement


v0v1 : DataV0 -> DataV1
v0v1 v0 =
    DataV1
        v0.rawEntries
        (Account.fromV0 v0.accounts)
        (Category.fromV0 v0.categories)
        (ImportProfile.fromV0 v0.importProfiles)
        v0.autoIncrement


empty : Data
empty =
    { rawEntries = RawEntry.empty
    , accounts = Dict.empty
    , categories = Dict.empty
    , importProfiles = Dict.empty
    , audits = Audits.empty
    , aggregationGroups = AggreationGroup.empty
    , autoIncrement = 0
    }


jsonEncoder : Data -> Value
jsonEncoder =
    S.encodeToJson dataCodec


jsonDecoder : Json.Decode.Decoder Data
jsonDecoder =
    S.getJsonDecoder never dataCodec



-- versioning-aware encoding


type StorageVersions
    = V0 DataV0
    | V1 DataV1
    | V2 DataV2
    | V3 DataV3
    | V4 DataV4


dataCodec : S.Codec e Data
dataCodec =
    S.customType
        (\v0Encoder v1Encoder v2Encoder v3Encoder v4Encoder value ->
            case value of
                V0 record ->
                    v0Encoder record

                V1 record ->
                    v1Encoder record

                V2 record ->
                    v2Encoder record

                V3 record ->
                    v3Encoder record

                V4 record ->
                    v4Encoder record
        )
        |> S.variant1 V0 v0Codec
        |> S.variant1 V1 v1Codec
        |> S.variant1 V2 v2Codec
        |> S.variant1 V3 v3Codec
        |> S.variant1 V4 v4Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        (v0v1 >> v1v2 >> v2v3 >> v3v4) storage

                    V1 storage ->
                        (v1v2 >> v2v3 >> v3v4) storage

                    V2 storage ->
                        (v2v3 >> v3v4) storage

                    V3 storage ->
                        v3v4 storage

                    V4 storage ->
                        storage
            )
            V4


v4Codec : S.Codec e DataV4
v4Codec =
    S.record DataV4
        |> S.field .rawEntries RawEntry.codec
        |> S.field .accounts Account.codec
        |> S.field .categories Category.codec
        |> S.field .importProfiles ImportProfile.codec
        |> S.field .audits Audits.codec
        |> S.field .aggregationGroups AggreationGroup.codec
        |> S.field .autoIncrement S.int
        |> S.finishRecord


v3Codec : S.Codec e DataV3
v3Codec =
    S.record DataV3
        |> S.field .rawEntries RawEntry.codec
        |> S.field .accounts Account.codec
        |> S.field .categories Category.codec
        |> S.field .importProfiles ImportProfile.codec
        |> S.field .audits Audits.codec
        |> S.field .autoIncrement S.int
        |> S.finishRecord


v2Codec : S.Codec e DataV2
v2Codec =
    S.record DataV2
        |> S.field .rawEntries RawEntry.codec
        |> S.field .accounts Account.codec
        |> S.field .categories Category.codec
        |> S.field .importProfiles ImportProfile.codec
        |> S.field .autoIncrement S.int
        |> S.finishRecord


v1Codec : S.Codec e DataV1
v1Codec =
    S.record DataV1
        |> S.field .rawEntries (S.dict S.string RawEntry.v0CodecVersioned)
        |> S.field .accounts Account.codec
        |> S.field .categories Category.codec
        |> S.field .importProfiles ImportProfile.codec
        |> S.field .autoIncrement S.int
        |> S.finishRecord


v0Codec : S.Codec e DataV0
v0Codec =
    S.record DataV0
        |> S.field .rawEntries (S.dict S.string RawEntry.v0Codec)
        |> S.field .accounts (S.dict S.int Account.v0Codec)
        |> S.field .categories (S.dict S.int Category.v0Codec)
        |> S.field .importProfiles (S.dict S.int ImportProfile.v0Codec)
        |> S.field .autoIncrement S.int
        |> S.finishRecord
