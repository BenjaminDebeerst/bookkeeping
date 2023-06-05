module Processing.Model exposing (getCategoryByShort, getEntries, getEntriesAndErrors)

import Dict exposing (Dict)
import Maybe.Extra
import Persistence.Data as Data exposing (Account, AccountStart, Category, Data, RawEntry)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Processing.CsvParser exposing (ParsedRow, parseCsvLine)
import Processing.Filter exposing (Filter, all)
import Processing.Ordering exposing (Ordering)
import Result.Extra


getEntries : Data -> List Filter -> Ordering BookEntry -> List BookEntry
getEntries data filters order =
    getEntriesAndErrors data filters order
        |> Tuple.first


getEntriesAndErrors : Data -> List Filter -> Ordering BookEntry -> ( List BookEntry, List String )
getEntriesAndErrors data filters order =
    Dict.values data.rawEntries
        |> List.map (parseToEntry data)
        |> List.map (Result.andThen (enrichRow data))
        |> Result.Extra.partition
        |> Tuple.mapFirst (List.filter (all filters))
        |> Tuple.mapFirst (List.sortWith order)


type alias Entry =
    { id : String
    , row : Result String ParsedRow
    , accountId : Int
    , categorization : Maybe Data.Categorization
    }


parseToEntry : Data -> RawEntry -> Result String Entry
parseToEntry data raw =
    Dict.get raw.importProfile data.importProfiles
        |> Maybe.map
            (\profile ->
                let
                    row =
                        parseCsvLine profile raw.line
                in
                Entry raw.id row profile.accountId raw.categorization
            )
        |> Result.fromMaybe "Import profile not found."


andMap =
    Result.map2 (|>)


enrichRow : Data -> Entry -> Result String BookEntry
enrichRow data entry =
    Ok BookEntry
        |> andMap (Ok entry.id)
        |> andMap (Result.map .date entry.row)
        |> andMap (Result.map .description entry.row)
        |> andMap (Result.map .amount entry.row)
        |> andMap (Dict.get entry.accountId data.accounts |> Result.fromMaybe ("Account not found: " ++ String.fromInt entry.accountId))
        |> andMap (liftCategorization data entry.categorization |> Result.fromMaybe "Category not found")


liftCategorization : Data -> Maybe Data.Categorization -> Maybe Categorization
liftCategorization data cat =
    case cat of
        Nothing ->
            Just None

        Just (Data.Single id) ->
            Dict.get id data.categories |> Maybe.map Single

        Just (Data.Split list) ->
            list
                |> List.map (liftSplitEntry data)
                |> Maybe.Extra.combine
                |> Maybe.map Split


liftSplitEntry : Data -> Data.SplitCatEntry -> Maybe EntrySplit
liftSplitEntry data se =
    Dict.get se.id data.categories |> Maybe.map (\c -> EntrySplit c se.amount)


getCategoryByShort : List Category -> String -> Maybe Category
getCategoryByShort categories string =
    categories
        |> List.filter (\c -> c.short == string)
        |> List.head
