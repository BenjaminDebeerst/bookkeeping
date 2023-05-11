module Processing.Filter exposing (..)

import Processing.BookEntry exposing (BookEntry)
import Time.Date as Date


type alias Filter =
    BookEntry -> Bool


all : List Filter -> Filter
all l =
    \e -> List.all (\f -> f e) l


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
