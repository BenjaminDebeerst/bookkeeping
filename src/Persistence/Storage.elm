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
    )

import Dict
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


addEntries : Bool -> List RawEntry -> Data -> Data
addEntries generateIds entries data =
    let
        ( newAutoIncrement, newEntries ) =
            entries
                |> List.foldl
                    (addEntryHelper generateIds)
                    ( data.autoIncrement, [] )
    in
    { data
        | rawEntries = Dict.union (Dict.fromList newEntries) data.rawEntries
        , autoIncrement = newAutoIncrement
    }


removeEntries : List String -> Data -> Data
removeEntries ids data =
    { data | rawEntries = Dict.Extra.removeMany (Set.fromList ids) data.rawEntries }


addEntryHelper : Bool -> RawEntry -> ( Int, List ( String, RawEntry ) ) -> ( Int, List ( String, RawEntry ) )
addEntryHelper generateIds entry ( i, acc ) =
    let
        ( j, id ) =
            if generateIds then
                ( i + 1, i |> String.fromInt |> sha1 )

            else
                ( i, entry.line |> sha1 )
    in
    ( j, [ ( id, { entry | id = id } ) ] ++ acc )


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
