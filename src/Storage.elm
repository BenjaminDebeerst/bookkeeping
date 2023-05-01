port module Storage exposing
    ( Storage
    , addEntries
    , decode
    , encode
    , hashData
    , loadDatabase
    , onChange
    , removeEntry
    , sha1
    , truncate
    )

import Csv exposing (Entry)
import Dict exposing (Dict)
import SHA1
import Serialize as S


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


type alias Storage =
    StorageV1


type alias StorageV1 =
    { bookEntries : Dict String Entry
    }


type alias StorageV0 =
    { rawData : Dict String String
    }


migrateV0 : StorageV0 -> StorageV1
migrateV0 v0 =
    StorageV1 <| Dict.fromList <| List.map (\e -> ( e.id, e )) <| Csv.validEntries v0.rawData


type StorageVersions
    = V0 StorageV0
    | V1 StorageV1


addEntries : Storage -> List Entry -> Cmd msg
addEntries storage lines =
    { storage | bookEntries = Dict.union storage.bookEntries <| Dict.fromList <| List.map (\e -> ( e.id, e )) lines }
        |> encode
        |> save


removeEntry : Storage -> String -> Cmd msg
removeEntry storage id =
    { storage | bookEntries = Dict.remove id storage.bookEntries } |> encode |> save


hashData : List String -> List ( String, String )
hashData lines =
    lines |> List.map String.trim |> List.map (\l -> ( sha1 l, l ))


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex |> String.slice 0 8


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    empty |> encode |> save


storageCodec : S.Codec e Storage
storageCodec =
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
                    V1 storage ->
                        storage

                    V0 storage ->
                        migrateV0 storage
            )
            V1


v1Codec : S.Codec e Storage
v1Codec =
    S.record StorageV1
        |> S.field .bookEntries
            (S.dict S.string
                (S.record Entry
                    |> S.field .id S.string
                    |> S.field .date S.string
                    |> S.field .description S.string
                    |> S.field .amount S.int
                    |> S.finishRecord
                )
            )
        |> S.finishRecord


v0Codec : S.Codec e StorageV0
v0Codec =
    S.record StorageV0
        |> S.field .rawData (S.dict S.string S.string)
        |> S.finishRecord


encode : Storage -> String
encode storage =
    S.encodeToString storageCodec storage


decode : String -> Storage
decode value =
    case S.decodeFromString storageCodec (String.trim value) of
        Ok data ->
            data

        Err _ ->
            empty


empty : Storage
empty =
    { bookEntries = Dict.empty }


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\s -> decode s |> fromStorage)
