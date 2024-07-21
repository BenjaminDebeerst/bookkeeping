module Util.YearMonth exposing (YearMonth, add, compare, components, fromDate, new, range, toDate, zero)

import Time.Date as Date exposing (Date)


{-| A combination of year and month, as the number of months since Jan 1970, which is the 0 value.
-}
type YearMonth
    = YM Int


new : Int -> Int -> YearMonth
new year month =
    Date.date year month 1 |> fromDate


zero : YearMonth
zero =
    YM 0


add : Int -> YearMonth -> YearMonth
add months (YM i) =
    YM (i + months)


compare : YearMonth -> YearMonth -> Order
compare (YM a) (YM b) =
    Basics.compare a b


range : List YearMonth -> List YearMonth
range yms =
    let
        is =
            yms |> List.map (\(YM i) -> i)

        iMin =
            List.minimum is

        iMax =
            List.maximum is
    in
    Maybe.map2 List.range iMin iMax |> Maybe.withDefault [] |> List.map YM


fromDate : Date -> YearMonth
fromDate date =
    YM ((Date.year date - 1970) * 12 + Date.month date - 1)


toDate : YearMonth -> Date
toDate yearMonth =
    let
        ( year, month ) =
            components yearMonth
    in
    Date.date year month 1


components : YearMonth -> ( Int, Int )
components (YM i) =
    let
        year =
            i // 12 + 1970

        month =
            remainderBy 12 i + 1
    in
    if month >= 1 then
        ( year, month )

    else
        -- for negative i, remainder is negative
        ( year - 1, month + 12 )
