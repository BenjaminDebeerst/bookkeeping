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
    , updateAudits
    , updateEntries
    )

import Dict exposing (Dict)
import Dict.Extra
import Persistence.Account exposing (Account)
import Persistence.Audits exposing (Audits)
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data, empty)
import Persistence.ImportProfile exposing (ImportProfile)
import Persistence.RawEntry exposing (RawEntries, RawEntry)
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


updateEntries : Dict Int RawEntry -> Data -> Data
updateEntries newEntries ({ rawEntries } as data) =
    let
        updatedEntries =
            Dict.union
                (newEntries |> Dict.filter (\k _ -> Dict.member k rawEntries.entries))
                rawEntries.entries
    in
    { data | rawEntries = { rawEntries | entries = updatedEntries } }


addEntries : List RawEntry -> Data -> Data
addEntries entries ({ rawEntries } as data) =
    let
        addEntryHelper entry ( i, acc ) =
            ( i + 1, [ ( i, { entry | id = i } ) ] ++ acc )

        ( newAutoIncrement, newEntries ) =
            List.foldl
                addEntryHelper
                ( rawEntries.autoIncrement, [] )
                entries

        updatedEntries =
            Dict.union (Dict.fromList newEntries) rawEntries.entries
    in
    { data | rawEntries = { rawEntries | entries = updatedEntries, autoIncrement = newAutoIncrement } }


removeEntries : List Int -> Data -> Data
removeEntries ids ({ rawEntries } as data) =
    let
        updatedEntries =
            Dict.Extra.removeMany (Set.fromList ids) rawEntries.entries
    in
    { data | rawEntries = { rawEntries | entries = updatedEntries } }


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


updateAudits : Audits -> Data -> Data
updateAudits audits data =
    { data | audits = audits }


truncate : Data
truncate =
    empty
