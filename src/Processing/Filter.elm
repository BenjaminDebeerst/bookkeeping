module Processing.Filter exposing (..)

import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Processing.BookEntry exposing (BookEntry)
import Regex
import Time.Date as Date exposing (Date)


type alias Filter =
    BookEntry -> Bool


all : List Filter -> Filter
all l =
    \e -> List.all (\f -> f e) l


any : List Filter -> Filter
any l =
    \e -> List.any (\f -> f e) l


filterDateRange : (Date -> Date -> Order) -> Date -> Date -> BookEntry -> Bool
filterDateRange order min max be =
    (not <| order min be.date == GT) && (not <| order be.date max == GT)


filterDescription : String -> BookEntry -> Bool
filterDescription s e =
    if String.isEmpty s then
        True

    else
        String.contains (String.toLower s) (String.toLower e.description)


filterDescriptionRegex : String -> BookEntry -> Bool
filterDescriptionRegex pattern e =
    case Regex.fromString pattern of
        Nothing ->
            False

        Just regex ->
            Regex.contains regex e.description


filterCategory : Category -> BookEntry -> Bool
filterCategory category bookEntry =
    case bookEntry.categorization of
        Processing.BookEntry.None ->
            False

        Processing.BookEntry.Single c ->
            c.id == category.id

        Processing.BookEntry.Split entrySplits ->
            List.map .category entrySplits |> List.filter (\c -> c.id == category.id) |> List.isEmpty >> not


filterAccount : Account -> BookEntry -> Bool
filterAccount account bookEntry =
    bookEntry.account == account
