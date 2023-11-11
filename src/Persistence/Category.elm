module Persistence.Category exposing (Categories, Category, CategoryV0, category, codec, fromV0, v0Codec)

import Dict exposing (Dict)
import Serialize as S


type alias Categories =
    Dict Int Category


type alias Category =
    CategoryV0


fromV0 : Dict Int CategoryV0 -> Categories
fromV0 dict =
    dict


type alias CategoryV0 =
    { id : Int
    , name : String
    , short : String
    }


category : Int -> String -> String -> Category
category i s1 s2 =
    CategoryV0 i s1 s2



-- Codecs


codec : S.Codec String Categories
codec =
    S.dict S.int categoryCodec


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


categoryCodec : S.Codec String Category
categoryCodec =
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
