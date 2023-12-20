module Persistence.CategorizationRule exposing (CategorizationRules, CategorizationRule, categorizationRule, codec)

import Dict exposing (Dict)
import Serialize as S


type alias CategorizationRules =
    Dict Int CategorizationRule


type alias CategorizationRule =
    CategorizationRuleV2


type alias CategorizationRuleV2 =
    { id : Int
    , pattern : String
    , category : Int
    }

categorizationRule : Int -> String -> Int -> CategorizationRule
categorizationRule =
    CategorizationRuleV2

-- versioning-aware encoding

codec : S.Codec String CategorizationRules
codec =
    S.dict S.int categorizationRuleCodec


categorizationRuleCodec : S.Codec String CategorizationRule
categorizationRuleCodec =
    S.customType
        (\v2Encoder value ->
            case value of
                V2 record ->
                    v2Encoder record
        )
        |> S.variant1 V2 v2Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V2 storage ->
                        storage
            )
            V2

type StorageVersions
    = V2 CategorizationRuleV2

v2Codec : S.Codec String CategorizationRule
v2Codec =
    S.record CategorizationRuleV2
        |> S.field .id S.int
        |> S.field .pattern S.string
        |> S.field .category S.int
        |> S.finishRecord
