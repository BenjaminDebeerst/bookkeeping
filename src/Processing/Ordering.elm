module Processing.Ordering exposing (..)

import Processing.BookEntry exposing (BookEntry)
import Time.Date as Date


type alias Ordering a =
    a -> a -> Basics.Order


asc : (BookEntry -> comparable) -> Ordering BookEntry
asc f =
    \e1 e2 -> compare (f e1) (f e2)


desc : (BookEntry -> comparable) -> Ordering BookEntry
desc f =
    \e1 e2 -> compare (f e2) (f e1)


dateAsc =
    asc (\e -> Date.toTuple e.date)


dateDesc =
    desc (\e -> Date.toTuple e.date)
