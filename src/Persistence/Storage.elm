module Persistence.Storage exposing
    ( addAccount
    , addCategories
    , addCategory
    , addEntries
    , addImportProfile
    , deleteAccount
    , deleteCategory
    , deleteImportProfile
    , editAccount
    , editCategory
    , editImportProfile
    , removeEntries
    , truncate
    , updateEntries
    )

import Dict exposing (Dict)
import Dict.Extra
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data, empty)
import Persistence.ImportProfile exposing (ImportProfile)
import Persistence.RawEntry exposing (RawEntry, sha1)
import Set


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


editAccount : Account -> Data -> Data
editAccount account data =
    { data | accounts = Dict.insert account.id account data.accounts }


deleteAccount : Account -> Data -> Data
deleteAccount account data =
    { data | accounts = Dict.remove account.id data.accounts }


updateEntries : Dict String RawEntry -> Data -> Data
updateEntries newEntries data =
    { data | rawEntries = Dict.union newEntries data.rawEntries }


addEntries : List RawEntry -> Data -> Data
addEntries entries data =
    let
        ( newAutoIncrement, newEntries ) =
            entries
                |> List.foldl
                    addEntryHelper
                    ( data.autoIncrement, [] )
    in
    { data
        | rawEntries = Dict.union (Dict.fromList newEntries) data.rawEntries
        , autoIncrement = newAutoIncrement
    }


removeEntries : List String -> Data -> Data
removeEntries ids data =
    { data | rawEntries = Dict.Extra.removeMany (Set.fromList ids) data.rawEntries }


addEntryHelper : RawEntry -> ( Int, List ( String, RawEntry ) ) -> ( Int, List ( String, RawEntry ) )
addEntryHelper entry ( i, acc ) =
    let
        id =
            i |> String.fromInt |> sha1
    in
    ( i + 1, [ ( id, { entry | id = id } ) ] ++ acc )


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


truncate : Data
truncate =
    empty
