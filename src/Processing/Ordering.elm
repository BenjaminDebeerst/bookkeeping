module Processing.Ordering exposing (Ordering, aggregateMonth, asc, bookEntryDate, desc)

import Processing.Aggregation exposing (MonthAggregate)
import Processing.BookEntry exposing (BookEntry)
import Time.Date as Date
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias Ordering a =
    a -> a -> Basics.Order


asc : (a -> comparable) -> Ordering a
asc f a1 a2 =
    compare (f a1) (f a2)


desc : (a -> comparable) -> Ordering a
desc f a1 a2 =
    compare (f a2) (f a1)


bookEntryDate : BookEntry -> ( Int, Int, Int )
bookEntryDate =
    .date >> Date.toTuple


aggregateMonth : MonthAggregate -> ( Int, Int )
aggregateMonth =
    .month >> YearMonth.components
