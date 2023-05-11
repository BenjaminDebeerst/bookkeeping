module Persistence.Data exposing
    ( Account
    , AccountStart
    , Categorization(..)
    , Category
    , Data
    , RawEntry
    , SplitCatEntry
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
    , categories : Dict Int Category
    , autoIncrement : Int
    }


type alias RawEntry =
    { id : String
    , line : String
    , accountId : Int
    , categorization : Maybe Categorization
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
    , categorization = Nothing
    }


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex


type alias Category =
    { id : Int
    , name : String
    , short : String
    }


type Categorization
    = Single Int
    | Split (List SplitCatEntry)


type alias SplitCatEntry =
    { id : Int, amount : Int }


empty : Data
empty =
    { rawEntries = Dict.empty
    , accounts = Dict.empty
    , categories = Dict.empty
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
        |> S.field .categories (S.dict S.int categoryCodec)
        |> S.field .autoIncrement S.int
        |> S.finishRecord


rawEntryCodec =
    S.record RawEntry
        |> S.field .id S.string
        |> S.field .line S.string
        |> S.field .accountId S.int
        |> S.field .categorization (S.maybe categorizationCodec)
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


categoryCodec : S.Codec e Category
categoryCodec =
    S.record Category
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.finishRecord


splitCategorizationCodec : S.Codec e (List SplitCatEntry)
splitCategorizationCodec =
    S.list
        (S.record SplitCatEntry
            |> S.field .id S.int
            |> S.field .amount S.int
            |> S.finishRecord
        )


categorizationCodec : S.Codec e Categorization
categorizationCodec =
    S.customType
        (\singleEncoder splitEncoder value ->
            case value of
                Single id ->
                    singleEncoder id

                Split lst ->
                    splitEncoder lst
        )
        |> S.variant1 Single S.int
        |> S.variant1 Split splitCategorizationCodec
        |> S.finishCustomType
