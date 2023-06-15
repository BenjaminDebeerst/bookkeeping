port module Persistence.Storage exposing
    ( addAccount
    , addCategories
    , addCategory
    , addEntries
    , addImportProfile
    , deleteCategory
    , deleteImportProfile
    , editCategory
    , editImportProfile
    , load
    , loadDatabase
    , onChange
    , save
    , store
    , truncate
    )

import Dict exposing (Dict)
import Persistence.Data exposing (Account, Category, Data, ImportProfile, RawEntry, decode, empty, encode)
import Serialize exposing (Error)


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


store : Data -> Cmd msg
store =
    encode >> save


onChange : (Result (Error String) Data -> msg) -> Sub msg
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


addCategories : List Category -> Data -> Data
addCategories categories data =
    List.foldl addCategory data categories


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


addImportProfile : ImportProfile -> Data -> Data
addImportProfile profile data =
    let
        id =
            data.autoIncrement
    in
    { data
        | importProfiles = Dict.insert id { profile | id = id } data.importProfiles
        , autoIncrement = id + 1
    }


editImportProfile : ImportProfile -> Data -> Data
editImportProfile profile data =
    { data | importProfiles = Dict.insert profile.id profile data.importProfiles }


deleteImportProfile : ImportProfile -> Data -> Data
deleteImportProfile profile data =
    { data | importProfiles = Dict.remove profile.id data.importProfiles }


loadDatabase : String -> Cmd msg
loadDatabase encodedStorage =
    encodedStorage |> save


truncate : Cmd msg
truncate =
    empty |> encode |> save
