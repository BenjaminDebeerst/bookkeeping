module Persistence.Category exposing (Categories, Category, CategoryV0, category, codec, fromV0, v0Codec)

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
    , long : String
    , anotherField : Int
    }


type alias CategoryV1 =
    { id : Int
    , name : String
    , short : String
    , long : String
    }


fromV0 : Dict Int CategoryV0 -> Categories
fromV0 dict =
    Dict.map (\_ -> v0v1 >> v1v2) dict


type alias CategoryV0 =
    { id : Int
    , name : String
    , short : String
    }


category : Int -> String -> String -> Category
category i s1 s2 =
    CategoryV2 i s1 s2 "" 0



-- Codecs


codec : S.Codec String Categories
codec =
    S.dict S.int categoryCodec


v2Codec : S.Codec String CategoryV2
v2Codec =
    S.record CategoryV2
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.field .long S.string
        |> S.field .anotherField S.int
        |> S.finishRecord


v1Codec : S.Codec String CategoryV1
v1Codec =
    S.record CategoryV1
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.field .long S.string
        |> S.finishRecord


v0Codec : S.Codec String CategoryV0
v0Codec =
    S.record CategoryV0
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.finishRecord



-- versioning-aware encoding


type StorageVersions
    = V0 CategoryV0
    | V1 CategoryV1
    | V2 CategoryV2


categoryCodec : S.Codec String Category
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
                        (v0v1 >> v1v2) storage

                    V1 storage ->
                        v1v2 storage

                    V2 storage ->
                        storage
            )
            V2


v0v1 : CategoryV0 -> CategoryV1
v0v1 v0 =
    CategoryV1 v0.id v0.name v0.short ""


v1v2 : CategoryV1 -> CategoryV2
v1v2 v1 =
    CategoryV2 v1.id v1.name v1.short v1.long 0
