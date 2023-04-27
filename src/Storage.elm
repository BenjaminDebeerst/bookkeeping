port module Storage exposing
    ( Storage
    , addRows
    , decode
    , encode
    , hashData
    , loadDatabase
    , onChange
    , remove
    , truncate
    )

import Dict exposing (Dict)
import SHA1
import Serialize as S


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


type alias Storage =
    { rawData : Dict String String
    }


addRows : Storage -> List String -> Cmd msg
addRows storage lines =
    { storage | rawData = Dict.union storage.rawData <| hashData lines }
        |> encode
        |> save


remove : Storage -> String -> Cmd msg
remove storage id =
    { storage | rawData = Dict.remove id storage.rawData } |> encode |> save


hashData : List String -> Dict String String
hashData lines =
    lines |> List.map String.trim |> List.map (\l -> ( sha1 l, l )) |> Dict.fromList


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex |> String.slice 0 8


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    empty |> encode |> save


dataCodec =
    S.record Storage
        |> S.field .rawData (S.dict S.string S.string)
        |> S.finishRecord


encode : Storage -> String
encode storage =
    S.encodeToString dataCodec storage


decode : String -> Storage
decode value =
    case S.decodeFromString dataCodec (String.trim value) of
        Ok data ->
            data

        Err _ ->
            empty


empty : Storage
empty =
    { rawData = Dict.empty }


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\s -> decode s |> fromStorage)
