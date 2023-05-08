port module Persistence.Storage exposing
    ( addAccount
    , addEntries
    , load
    , loadDatabase
    , onChange
    , removeEntry
    , save
    , truncate
    )

import Dict exposing (Dict)
import Persistence.Data as Storage exposing (Account, Data, RawAccountEntry, decode, encode)


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


onChange : (Data -> msg) -> Sub msg
onChange fromStorage =
    load (\s -> decode s |> fromStorage)


addAccount : Data -> Account -> Cmd msg
addAccount data account =
    let
        id =
            data.autoIncrement
    in
    { data
        | accounts = Dict.insert id { account | id = id } data.accounts
        , autoIncrement = id + 1
    }
        |> encode
        |> save


addEntries : Data -> List RawAccountEntry -> Cmd msg
addEntries data lines =
    { data | rawEntries = Dict.union data.rawEntries <| Dict.fromList <| List.map (\e -> ( e.entry.id, e )) lines }
        |> encode
        |> save


removeEntry : Data -> String -> Cmd msg
removeEntry data id =
    { data | rawEntries = Dict.remove id data.rawEntries } |> encode |> save


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    Storage.empty |> encode |> save
