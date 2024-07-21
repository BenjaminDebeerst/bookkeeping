module Processing.Filter exposing (..)

import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Processing.Aggregation exposing (Aggregate)
import Processing.BookEntry exposing (BookEntry)
import Regex
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias EntryFilter =
    BookEntry -> Bool


type alias AggregateFilter =
    Aggregate -> Aggregate


all : List EntryFilter -> EntryFilter
all l =
    \e -> List.all (\f -> f e) l


any : List EntryFilter -> EntryFilter
any l =
    \e -> List.any (\f -> f e) l


inInclusiveRange : YearMonth -> YearMonth -> YearMonth -> Bool
inInclusiveRange min max candidate =
    not (YearMonth.compare min candidate == GT || YearMonth.compare max candidate == LT)


filterDateRange : YearMonth -> YearMonth -> EntryFilter
filterDateRange min max be =
    inInclusiveRange min max (YearMonth.fromDate be.date)


filterAggregateDateRange : YearMonth -> YearMonth -> Aggregate -> Aggregate
filterAggregateDateRange min max ag =
    { ag | rows = ag.rows |> List.filter (.month >> inInclusiveRange min max) }


filterDescription : String -> EntryFilter
filterDescription s e =
    if String.isEmpty s then
        True

    else
        String.contains (String.toLower s) (String.toLower e.description)


filterDescriptionRegex : String -> EntryFilter
filterDescriptionRegex pattern e =
    case Regex.fromString pattern of
        Nothing ->
            False

        Just regex ->
            Regex.contains regex e.description


filterCategory : Category -> EntryFilter
filterCategory category bookEntry =
    case bookEntry.categorization of
        Processing.BookEntry.None ->
            False

        Processing.BookEntry.Single c ->
            c.id == category.id

        Processing.BookEntry.Split entrySplits ->
            List.map .category entrySplits |> List.filter (\c -> c.id == category.id) |> List.isEmpty >> not


filterAccount : Account -> EntryFilter
filterAccount account bookEntry =
    bookEntry.account == account
