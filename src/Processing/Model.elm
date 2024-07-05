module Processing.Model exposing (getCategoryByShort, getCategoryForParsedRow, getEntries, getEntriesAndErrors)

import Dict exposing (Dict)
import Maybe.Extra
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data)
import Persistence.RawEntry as RawEntry exposing (RawEntry)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Processing.CsvParser exposing (ParsedRow)
import Processing.Filter exposing (EntryFilter, all)
import Processing.Ordering exposing (Ordering)
import Result.Extra


getEntries : Data -> List EntryFilter -> Ordering BookEntry -> List BookEntry
getEntries data filters order =
    getEntriesAndErrors data filters order
        |> Tuple.first


getEntriesAndErrors : Data -> List EntryFilter -> Ordering BookEntry -> ( List BookEntry, List String )
getEntriesAndErrors data filters order =
    Dict.values data.rawEntries.entries
        |> List.map (enrichRow data)
        |> Result.Extra.partition
        |> Tuple.mapFirst (List.filter (all filters))
        |> Tuple.mapFirst (List.sortWith order)


andMap =
    Result.map2 (|>)


enrichRow : Data -> RawEntry -> Result String BookEntry
enrichRow data entry =
    Ok (BookEntry entry.id entry.date entry.description entry.amount entry.comment)
        |> andMap (Dict.get entry.accountId data.accounts |> Result.fromMaybe ("Account not found: " ++ String.fromInt entry.accountId))
        |> andMap (liftCategorization data entry.categorization |> Result.fromMaybe "Category not found")


liftCategorization : Data -> Maybe RawEntry.Categorization -> Maybe Categorization
liftCategorization data cat =
    case cat of
        Nothing ->
            Just None

        Just (RawEntry.Single id) ->
            Dict.get id data.categories |> Maybe.map Single

        Just (RawEntry.Split list) ->
            list
                |> List.map (liftSplitEntry data)
                |> Maybe.Extra.combine
                |> Maybe.map Split


liftSplitEntry : Data -> RawEntry.SplitCatEntry -> Maybe EntrySplit
liftSplitEntry data se =
    Dict.get se.id data.categories |> Maybe.map (\c -> EntrySplit c se.amount)


getCategoryByShort : List Category -> String -> Maybe Category
getCategoryByShort categories string =
    let
        searchShort =
            String.toUpper string
    in
    categories
        |> List.filter (\c -> c.short == searchShort)
        |> List.head


getCategoryForParsedRow : (String -> Maybe Category) -> List Category -> ParsedRow -> Maybe Category
getCategoryForParsedRow categorizationRules categories row =
    case Maybe.andThen (getCategoryByShort categories) row.category of
        Nothing ->
            categorizationRules row.description

        Just c ->
            Just c
