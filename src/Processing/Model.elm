module Processing.Model exposing (getCategoryByShort, getEntries)

import Dict exposing (Dict)
import Maybe.Extra
import Persistence.Data as Data exposing (Account, AccountStart, Category, Data, RawEntry)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Processing.Csv exposing (Row, parseCsvLine)
import Processing.Filter exposing (Filter, all)
import Processing.Ordering exposing (Ordering)


getEntries : Data -> List Filter -> Ordering BookEntry -> List BookEntry
getEntries data filters order =
    Dict.values data.rawEntries
        |> List.map parseToEntry
        |> List.filterMap (enrichRow data)
        |> List.filter (all filters)
        |> List.sortWith order


type alias Entry =
    { id : String
    , row : Maybe Row
    , accountId : Int
    , categorization : Maybe Data.Categorization
    }


parseToEntry : RawEntry -> Entry
parseToEntry raw =
    parseCsvLine raw.line
        |> Result.toMaybe
        |> (\row -> Entry raw.id row raw.accountId raw.categorization)


andMap =
    Maybe.map2 (|>)


enrichRow : Data -> Entry -> Maybe BookEntry
enrichRow data entry =
    Just BookEntry
        |> andMap (Just entry.id)
        |> andMap (Maybe.map .date entry.row)
        |> andMap (Maybe.map .description entry.row)
        |> andMap (Maybe.map .amount entry.row)
        |> andMap (Dict.get entry.accountId data.accounts)
        |> andMap (liftCategorization data entry.categorization)


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


getCategoryByShort : Data -> String -> Maybe Category
getCategoryByShort data string =
    Dict.values data.categories
        |> List.filter (\c -> c.short == string)
        |> List.head
