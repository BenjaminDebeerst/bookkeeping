port module Persistence.Storage exposing
    ( addEntries
    , load
    , loadDatabase
    , onChange
    , removeEntry
    , save
    , truncate
    )

import Dict exposing (Dict)
import Persistence.Data as Storage exposing (Data, Entry, decode, encode)


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


onChange : (Data -> msg) -> Sub msg
onChange fromStorage =
    load (\s -> decode s |> fromStorage)


addEntries : Data -> List Entry -> Cmd msg
addEntries data lines =
    { data | bookEntries = Dict.union data.bookEntries <| Dict.fromList <| List.map (\e -> ( e.id, e )) lines }
        |> encode
        |> save


removeEntry : Data -> String -> Cmd msg
removeEntry data id =
    { data | bookEntries = Dict.remove id data.bookEntries } |> encode |> save


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    Storage.empty |> encode |> save
