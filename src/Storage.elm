port module Storage exposing
    ( CsvLine
    , Storage
    , addRows
    , decode
    , encode
    , loadDatabase
    , onChange
    , truncate
    )

import Serialize as S


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


type alias CsvLine =
    ( String, String )


type alias Storage =
    { rawData : List CsvLine
    }


addRows : Storage -> List CsvLine -> Cmd msg
addRows storage lines =
    { storage | rawData = storage.rawData ++ lines }
        |> encode
        |> save


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    empty |> encode |> save


dataCodec =
    S.record Storage
        |> S.field .rawData (S.list <| S.tuple S.string S.string)
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
    { rawData = [] }


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\s -> decode s |> fromStorage)
