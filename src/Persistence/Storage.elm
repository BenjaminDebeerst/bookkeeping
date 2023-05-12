port module Persistence.Storage exposing
    ( addAccount
    , addEntries
    , load
    , loadDatabase
    , onChange
    , save
    , store
    , truncate
    )

import Dict exposing (Dict)
import Persistence.Data exposing (Account, Data, RawEntry, decode, empty, encode)


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


store : Data -> Cmd msg
store =
    encode >> save


onChange : (Data -> msg) -> Sub msg
onChange fromStorage =
    load (\s -> decode s |> fromStorage)


addAccount : Account -> Data -> Data
addAccount account data =
    let
        id =
            data.autoIncrement
    in
    { data
        | accounts = Dict.insert id { account | id = id } data.accounts
        , autoIncrement = id + 1
    }


addEntries : List RawEntry -> Data -> Data
addEntries lines data =
    let
        newEntries =
            List.map (\e -> ( e.id, e )) lines |> Dict.fromList
    in
    { data | rawEntries = Dict.union data.rawEntries newEntries }


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    empty |> encode |> save
