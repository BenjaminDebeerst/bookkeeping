module Persistence.Category exposing (Categories, Category, CategoryGroup(..), CategoryV0, category, codec, fromV0, v0Codec)

import Dict exposing (Dict)
import Serialize as S


type alias Categories =
    Dict Int Category


type alias Category =
    CategoryV2


type alias CategoryV2 =
    { id : Int
    , name : String
    , short : String
    , group : CategoryGroup
    , rules : List String
    }


type alias CategoryV1 =
    { id : Int
    , name : String
    , short : String
    , group : CategoryGroup
    }


type CategoryGroup
    = Income
    | Expense
    | Internal


type alias CategoryV0 =
    { id : Int
    , name : String
    , short : String
    }


category : Int -> String -> String -> CategoryGroup -> List String -> Category
category =
    CategoryV2


fromV0 : Dict Int CategoryV0 -> Categories
fromV0 dict =
    Dict.map (\_ c -> v1v2 (v0v1 c)) dict


v1v2 : CategoryV1 -> CategoryV2
v1v2 c =
    CategoryV2 c.id c.name c.short c.group []


v0v1 : CategoryV0 -> CategoryV1
v0v1 c =
    -- using Expense as the default, that's the most common case
    CategoryV1 c.id c.name c.short Expense



-- versioning-aware encoding


codec : S.Codec e Categories
codec =
    S.dict S.int categoryCodec


categoryCodec : S.Codec e Category
categoryCodec =
    S.customType
        (\v0Encoder v1Encoder v2Encoder value ->
            case value of
                V0 record ->
                    v0Encoder record

                V1 record ->
                    v1Encoder record

                V2 record ->
                    v2Encoder record
        )
        |> S.variant1 V0 v0Codec
        |> S.variant1 V1 v1Codec
        |> S.variant1 V2 v2Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        v1v2 (v0v1 storage)

                    V1 storage ->
                        v1v2 storage

                    V2 storage ->
                        storage
            )
            V2


type StorageVersions
    = V0 CategoryV0
    | V1 CategoryV1
    | V2 CategoryV2


v2Codec : S.Codec e CategoryV2
v2Codec =
    S.record CategoryV2
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.field .group categoryGroupCodec
        |> S.field .rules (S.list S.string)
        |> S.finishRecord


v1Codec : S.Codec e CategoryV1
v1Codec =
    S.record CategoryV1
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.field .group categoryGroupCodec
        |> S.finishRecord


categoryGroupCodec : S.Codec e CategoryGroup
categoryGroupCodec =
    S.customType
        (\a b c value ->
            case value of
                Income ->
                    a

                Expense ->
                    b

                Internal ->
                    c
        )
        |> S.variant0 Income
        |> S.variant0 Expense
        |> S.variant0 Internal
        |> S.finishCustomType


v0Codec : S.Codec e CategoryV0
v0Codec =
    S.record CategoryV0
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.finishRecord
