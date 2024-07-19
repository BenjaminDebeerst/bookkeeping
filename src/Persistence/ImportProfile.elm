module Persistence.ImportProfile exposing (AmountField(..), DateFormat(..), ImportProfile, ImportProfileV0, ImportProfiles, codec, fromV0, importProfile, v0Codec)

import Dict exposing (Dict)
import Serialize as S


type alias ImportProfiles =
    Dict Int ImportProfile


type alias ImportProfile =
    ImportProfileV1


type alias ImportProfileV1 =
    { id : Int
    , name : String
    , splitAt : Char
    , dateField : Int
    , descrFields : List Int
    , amountField : AmountField
    , dateFormat : DateFormat
    , categoryField : Maybe Int
    }


type alias ImportProfileV0 =
    { id : Int
    , name : String
    , splitAt : Char
    , dateField : Int
    , descrFields : List Int
    , amountField : Int
    , dateFormat : DateFormat
    , categoryField : Maybe Int
    }


type DateFormat
    = YYYYMMDD Char
    | DDMMYYYY Char


type AmountField
    = Simple Int
    | Split Int Int


importProfile : Int -> String -> Char -> Int -> List Int -> AmountField -> DateFormat -> Maybe Int -> ImportProfile
importProfile id name split date desc amnt dateFmt cat =
    ImportProfileV1 id name split date desc amnt dateFmt cat


fromV0 : Dict Int ImportProfileV0 -> ImportProfiles
fromV0 dict =
    dict |> Dict.map (\_ -> v0toV1)


v0toV1 : ImportProfileV0 -> ImportProfileV1
v0toV1 v0 =
    ImportProfileV1
        v0.id
        v0.name
        v0.splitAt
        v0.dateField
        v0.descrFields
        (Simple v0.amountField)
        v0.dateFormat
        v0.categoryField



-- versioning-aware encoding


codec : S.Codec e ImportProfiles
codec =
    S.dict S.int profileCodec


profileCodec : S.Codec e ImportProfile
profileCodec =
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
                        v0toV1 storage

                    V1 storage ->
                        storage
            )
            V1


type StorageVersions
    = V0 ImportProfileV0
    | V1 ImportProfileV1


v1Codec : S.Codec e ImportProfileV1
v1Codec =
    S.record ImportProfileV1
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .splitAt charCodec
        |> S.field .dateField S.int
        |> S.field .descrFields (S.list S.int)
        |> S.field .amountField amountCodec
        |> S.field .dateFormat dateFormatCodec
        |> S.field .categoryField (S.maybe S.int)
        |> S.finishRecord


amountCodec : S.Codec e AmountField
amountCodec =
    S.customType
        (\simpleEncoder splitEncoder value ->
            case value of
                Simple i ->
                    simpleEncoder i

                Split c d ->
                    splitEncoder c d
        )
        |> S.variant1 Simple S.int
        |> S.variant2 Split S.int S.int
        |> S.finishCustomType


v0Codec : S.Codec e ImportProfileV0
v0Codec =
    S.record ImportProfileV0
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .splitAt charCodec
        |> S.field .dateField S.int
        |> S.field .descrFields (S.list S.int)
        |> S.field .amountField S.int
        |> S.field .dateFormat dateFormatCodec
        |> S.field .categoryField (S.maybe S.int)
        |> S.finishRecord


dateFormatCodec : S.Codec e DateFormat
dateFormatCodec =
    S.customType
        (\a b value ->
            case value of
                YYYYMMDD char ->
                    a char

                DDMMYYYY char ->
                    b char
        )
        |> S.variant1 YYYYMMDD charCodec
        |> S.variant1 DDMMYYYY charCodec
        |> S.finishCustomType


{-| Just store the char as 1-character string. The default value when decoding doesn't matter.
-}
charCodec : S.Codec e Char
charCodec =
    S.string |> S.map (\s -> s |> String.toList |> List.head |> Maybe.withDefault '?') String.fromChar
