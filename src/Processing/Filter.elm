module Processing.Filter exposing (..)

import Persistence.Data exposing (Account, Category)
import Processing.BookEntry exposing (BookEntry)
import Time.Date as Date


type alias Filter =
    BookEntry -> Bool


all : List Filter -> Filter
all l =
    \e -> List.all (\f -> f e) l


any : List Filter -> Filter
any l =
    \e -> List.any (\f -> f e) l


filterMonth : Int -> BookEntry -> Bool
filterMonth i e =
    Date.month e.date == i


filterYear : Int -> BookEntry -> Bool
filterYear i e =
    Date.year e.date == i


filterDescription : String -> BookEntry -> Bool
filterDescription s e =
    if String.isEmpty s then
        True

    else
        String.contains (String.toLower s) (String.toLower e.description)


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
