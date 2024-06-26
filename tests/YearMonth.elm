module YearMonth exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Time.Date as Date
import Util.YearMonth as YearMonth


components : Test
components =
    describe "Year Month components"
        [ test "2024-07" <| \_ -> YearMonth.components (YearMonth.new 2024 7) |> Expect.equal ( 2024, 7 )
        , test "1968-12" <| \_ -> YearMonth.components (YearMonth.new 1968 12) |> Expect.equal ( 1968, 12 )
        , test "1969-01" <| \_ -> YearMonth.components (YearMonth.new 1969 1) |> Expect.equal ( 1969, 1 )
        , test "1969-02" <| \_ -> YearMonth.components (YearMonth.new 1969 2) |> Expect.equal ( 1969, 2 )
        , test "1969-11" <| \_ -> YearMonth.components (YearMonth.new 1969 11) |> Expect.equal ( 1969, 11 )
        , test "1969-12" <| \_ -> YearMonth.components (YearMonth.new 1969 12) |> Expect.equal ( 1969, 12 )
        , test "1970-01" <| \_ -> YearMonth.components (YearMonth.new 1970 1) |> Expect.equal ( 1970, 1 )
        , test "1970-02" <| \_ -> YearMonth.components (YearMonth.new 1970 2) |> Expect.equal ( 1970, 2 )
        , test "1970-12" <| \_ -> YearMonth.components (YearMonth.new 1970 12) |> Expect.equal ( 1970, 12 )
        , test "1971-01" <| \_ -> YearMonth.components (YearMonth.new 1971 1) |> Expect.equal ( 1971, 1 )
        ]


dates : Test
dates =
    describe "YearMonth and dates"
        [ test "To date and back are inverse (1)" <|
            \_ -> YearMonth.new 1234 1 |> YearMonth.toDate |> YearMonth.fromDate |> Expect.equal (YearMonth.new 1234 1)
        , test "To date and back are inverse (2)" <|
            \_ -> YearMonth.new 2345 12 |> YearMonth.toDate |> YearMonth.fromDate |> Expect.equal (YearMonth.new 2345 12)
        , test "To yearMonth and back are is inverse for month and year, strips day (1)" <|
            \_ -> Date.date 1403 4 1 |> YearMonth.fromDate |> YearMonth.toDate |> Expect.equal (Date.date 1403 4 1)
        , test "To yearMonth and backare is inverse for month and year, strips day (2)" <|
            \_ -> Date.date 1998 8 10 |> YearMonth.fromDate |> YearMonth.toDate |> Expect.equal (Date.date 1998 8 1)
        , test "To yearMonth and backare is inverse for month and year, strips day (3)" <|
            \_ -> Date.date 2023 12 31 |> YearMonth.fromDate |> YearMonth.toDate |> Expect.equal (Date.date 2023 12 1)
        ]
