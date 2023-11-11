module Persistence.ImportProfile exposing (DateFormat(..), ImportProfile, ImportProfileV0, ImportProfiles, codec, fromV0, importProfile, v0Codec)

import Dict exposing (Dict)
import Serialize as S


type alias ImportProfiles =
    Dict Int ImportProfile


type alias ImportProfile =
    ImportProfileV0


fromV0 : Dict Int ImportProfileV0 -> ImportProfiles
fromV0 dict =
    dict


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


importProfile : Int -> String -> Char -> Int -> List Int -> Int -> DateFormat -> Maybe Int -> ImportProfile
importProfile =
    ImportProfileV0



-- Codecs


codec : S.Codec String ImportProfiles
codec =
    S.dict S.int profileCodec


v0Codec : S.Codec String ImportProfileV0
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


dateFormatCodec : S.Codec String DateFormat
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
charCodec : S.Codec String Char
charCodec =
    S.string |> S.map (\s -> s |> String.toList |> List.head |> Maybe.withDefault '?') String.fromChar



-- versioning-aware encoding


type StorageVersions
    = V0 ImportProfileV0


profileCodec : S.Codec String ImportProfile
profileCodec =
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
