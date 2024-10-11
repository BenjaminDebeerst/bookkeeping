module Processing.Filter exposing (..)

import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Processing.Aggregation exposing (MonthAggregate)
import Processing.BookEntry exposing (BookEntry)
import Processing.CsvParser exposing (ParsedRow)
import Regex
import Time.Date as Date exposing (Date)
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias EntryFilter =
    BookEntry -> Bool


type alias AggregateFilter =
    MonthAggregate -> Bool


type alias ParsedRowFilter =
    ParsedRow -> Bool


all : List EntryFilter -> EntryFilter
all l =
    \e -> List.all (\f -> f e) l


any : List EntryFilter -> EntryFilter
any l =
    \e -> List.any (\f -> f e) l


inInclusiveMonthRange : YearMonth -> YearMonth -> YearMonth -> Bool
inInclusiveMonthRange min max candidate =
    not (YearMonth.compare min candidate == GT || YearMonth.compare max candidate == LT)


inInclusiveDateRange : Date -> Date -> Date -> Bool
inInclusiveDateRange min max candidate =
    not (Date.compare min candidate == GT || Date.compare max candidate == LT)


filterEntryMonthRange : YearMonth -> YearMonth -> EntryFilter
filterEntryMonthRange min max be =
    inInclusiveMonthRange min max (YearMonth.fromDate be.date)


filterAggregateMonthRange : YearMonth -> YearMonth -> AggregateFilter
filterAggregateMonthRange min max monthAggregate =
    inInclusiveMonthRange min max monthAggregate.month


filterParsedRowDateRange : Date -> Date -> ParsedRowFilter
filterParsedRowDateRange min max row =
    inInclusiveDateRange min max row.date


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
