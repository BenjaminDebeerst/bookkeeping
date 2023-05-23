port module Persistence.Storage exposing
    ( addAccount
    , addCategory
    , addEntries
    , deleteCategory
    , editCategory
    , load
    , loadDatabase
    , onChange
    , save
    , store
    , truncate
    )

import Dict exposing (Dict)
import Persistence.Data exposing (Account, Category, Data, RawEntry, decode, empty, encode)


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
    { data | rawEntries = Dict.union newEntries data.rawEntries }


addCategory : Category -> Data -> Data
addCategory category data =
    let
        id =
            data.autoIncrement
    in
    { data
        | categories = Dict.insert id { category | id = id } data.categories
        , autoIncrement = id + 1
    }


editCategory : Category -> Data -> Data
editCategory category data =
    { data | categories = Dict.insert category.id category data.categories }


deleteCategory : Category -> Data -> Data
deleteCategory category data =
    { data | categories = Dict.remove category.id data.categories }


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    empty |> encode |> save
