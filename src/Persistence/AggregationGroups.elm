module Persistence.AggregationGroups exposing (AggregationGroup, AggregationGroups, add, codec, empty, get, getAll, remove, update)

import Dict exposing (Dict)
import Persistence.Category exposing (Category)
import Serialize as S


type alias AggregationGroups =
    AggregationGroupsV0


type AggregationGroupsV0
    = AggregationGroupsV0 Int (Dict Int AggregationGroupV0)


type alias AggregationGroup =
    AggregationGroupV0


type alias AggregationGroupV0 =
    { id : Int
    , name : String
    , categories : List Int
    }


add : String -> List Category -> AggregationGroups -> AggregationGroups
add name categories (AggregationGroupsV0 autoInc data) =
    AggregationGroupsV0 (autoInc + 1)
        (Dict.insert autoInc
            (AggregationGroupV0
                autoInc
                name
                (List.map .id categories)
            )
            data
        )


update : Int -> String -> List Category -> AggregationGroups -> AggregationGroups
update id name categories (AggregationGroupsV0 autoInc data) =
    AggregationGroupsV0 autoInc
        (Dict.update id
            (\_ ->
                Just
                    (AggregationGroupV0
                        id
                        name
                        (List.map .id categories)
                    )
            )
            data
        )


remove : Int -> AggregationGroups -> AggregationGroups
remove id (AggregationGroupsV0 autoInc data) =
    AggregationGroupsV0 autoInc (Dict.remove id data)


getAll : AggregationGroups -> List AggregationGroup
getAll (AggregationGroupsV0 _ data) =
    Dict.values data


get : Int -> AggregationGroups -> Maybe AggregationGroup
get id (AggregationGroupsV0 _ data) =
    Dict.get id data


empty : AggregationGroupsV0
empty =
    AggregationGroupsV0 0 Dict.empty



-- version-aware encoding


type StorageVersions
    = V0 AggregationGroupsV0


codec : S.Codec e AggregationGroups
codec =
    S.customType
        (\v0Encoder value ->
            case value of
                V0 record ->
                    v0Encoder record
        )
        |> S.variant1 V0 v0GroupsCodec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        storage
            )
            V0


v0GroupsCodec : S.Codec e AggregationGroupsV0
v0GroupsCodec =
    S.customType
        (\encoder (AggregationGroupsV0 autoIncrement data) -> encoder autoIncrement data)
        |> S.variant2 AggregationGroupsV0 S.int (S.dict S.int v0GroupCodec)
        |> S.finishCustomType


v0GroupCodec : S.Codec e AggregationGroupV0
v0GroupCodec =
    S.record AggregationGroupV0
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .categories (S.list S.int)
        |> S.finishRecord
