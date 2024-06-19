module Persistence.RawEntry exposing (Categorization(..), RawEntries, RawEntry, RawEntryV0, RawEntryV1, SplitCatEntry, codec, empty, fromV0, rawEntry, v0Codec, v0CodecVersioned)

import Dict exposing (Dict)
import Persistence.Category exposing (Category)
import Serialize as S
import Time.Date as Date exposing (Date)


type alias RawEntries =
    RawEntriesV2


type alias RawEntry =
    RawEntryV2


type alias RawEntriesV2 =
    { autoIncrement : Int
    , entries : Dict Int RawEntryV2
    }


type alias RawEntriesV1 =
    { autoIncrement : Int
    , entries : Dict Int RawEntryV1
    }


type alias RawEntriesV0 =
    Dict String RawEntryV0


type alias RawEntryV2 =
    { id : Int
    , line : String
    , date : Date
    , amount : Int
    , description : String
    , accountId : Int
    , importProfile : Int
    , categorization : Maybe Categorization
    , comment : String
    }


type alias RawEntryV1 =
    { id : Int
    , line : String
    , date : Date
    , amount : Int
    , description : String
    , accountId : Int
    , importProfile : Int
    , categorization : Maybe Categorization
    }


type alias RawEntryV0 =
    { id : String
    , line : String
    , date : Date
    , amount : Int
    , description : String
    , accountId : Int
    , importProfile : Int
    , categorization : Maybe Categorization
    }


type Categorization
    = Single Int
    | Split (List SplitCatEntry)


type alias SplitCatEntry =
    { id : Int, amount : Int }


empty : RawEntries
empty =
    RawEntriesV2 0 Dict.empty


rawEntry : Int -> Int -> String -> Date -> Int -> String -> Maybe Category -> RawEntry
rawEntry accountId profileId line date amount description category =
    RawEntryV2
        -1
        line
        date
        amount
        description
        accountId
        profileId
        (Maybe.map (.id >> Single) category)
        ""


v1ToV2 : RawEntriesV1 -> RawEntriesV2
v1ToV2 rawEntriesV1 =
    let
        mappedEntries : Dict Int RawEntryV2
        mappedEntries =
            rawEntriesV1.entries
                |> Dict.map (\_ e -> RawEntryV2 e.id e.line e.date e.amount e.description e.accountId e.importProfile e.categorization "")
    in
    RawEntriesV2 rawEntriesV1.autoIncrement mappedEntries


v0ToV1 : Dict String RawEntryV0 -> RawEntriesV1
v0ToV1 dict =
    let
        addEntryHelper e ( i, acc ) =
            ( i + 1
            , ( i
              , RawEntryV1 i e.line e.date e.amount e.description e.accountId e.importProfile e.categorization
              )
                :: acc
            )

        ( autoIncrement, entries ) =
            List.foldl
                addEntryHelper
                ( 0, [] )
                (Dict.values dict)
    in
    RawEntriesV1 autoIncrement (Dict.fromList entries)


fromV0 : Dict String RawEntryV0 -> RawEntries
fromV0 =
    v0ToV1 >> v1ToV2



-- versioning-aware encoding


codec : S.Codec e RawEntries
codec =
    S.customType
        (\v1Encoder v2Encoder value ->
            case value of
                V1 record ->
                    v1Encoder record

                V2 record ->
                    v2Encoder record
        )
        |> S.variant1 V1
            (S.record RawEntriesV1
                |> S.field .autoIncrement S.int
                |> S.field .entries (S.dict S.int v1Codec)
                |> S.finishRecord
            )
        |> S.variant1 V2
            (S.record RawEntriesV2
                |> S.field .autoIncrement S.int
                |> S.field .entries (S.dict S.int v2Codec)
                |> S.finishRecord
            )
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V1 storage ->
                        v1ToV2 storage

                    V2 storage ->
                        storage
            )
            V2


type RawEntriesVersions
    = V1 RawEntriesV1
    | V2 RawEntriesV2


v2Codec : S.Codec e RawEntryV2
v2Codec =
    S.record RawEntryV2
        |> S.field .id S.int
        |> S.field .line S.string
        |> S.field .date dateCodec
        |> S.field .amount S.int
        |> S.field .description S.string
        |> S.field .accountId S.int
        |> S.field .importProfile S.int
        |> S.field .categorization (S.maybe categorizationCodec)
        |> S.field .comment S.string
        |> S.finishRecord


v1Codec : S.Codec e RawEntryV1
v1Codec =
    S.record RawEntryV1
        |> S.field .id S.int
        |> S.field .line S.string
        |> S.field .date dateCodec
        |> S.field .amount S.int
        |> S.field .description S.string
        |> S.field .accountId S.int
        |> S.field .importProfile S.int
        |> S.field .categorization (S.maybe categorizationCodec)
        |> S.finishRecord


type RawEntryVersions
    = V0 RawEntryV0


v0CodecVersioned : S.Codec e RawEntryV0
v0CodecVersioned =
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


v0Codec : S.Codec e RawEntryV0
v0Codec =
    S.record RawEntryV0
        |> S.field .id S.string
        |> S.field .line S.string
        |> S.field .date dateCodec
        |> S.field .amount S.int
        |> S.field .description S.string
        |> S.field .accountId S.int
        |> S.field .importProfile S.int
        |> S.field .categorization (S.maybe categorizationCodec)
        |> S.finishRecord


dateCodec : S.Codec e Date
dateCodec =
    S.triple S.int S.int S.int
        |> S.map Date.fromTuple Date.toTuple


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


splitCategorizationCodec : S.Codec e (List SplitCatEntry)
splitCategorizationCodec =
    S.list
        (S.record SplitCatEntry
            |> S.field .id S.int
            |> S.field .amount S.int
            |> S.finishRecord
        )
