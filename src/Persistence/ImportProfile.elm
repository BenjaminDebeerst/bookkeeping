module Persistence.ImportProfile exposing (..)

import Dict exposing (Dict)
import Serialize as S


type alias ImportProfiles =
    Dict Int ImportProfile


type alias ImportProfile =
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



-- Codecs


codec : S.Codec String ImportProfiles
codec =
    S.dict S.int profileCodec


profileCodec : S.Codec String ImportProfile
profileCodec =
    S.record ImportProfile
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
