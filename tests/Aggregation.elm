module Aggregation exposing (..)

import Dict
import Expect exposing (Expectation)
import Persistence.Data exposing (Category)
import Processing.Aggregation exposing (MonthAggregate, aggregate)
import Processing.BookEntry exposing (BookEntry, Categorization(..))
import Test exposing (..)
import Time.Date as Date


cat1 =
    Category 1 "Cat 1" "CAT1"


cat2 =
    Category 2 "Cat 2" "CAT2"


categories : List Category
categories =
    [ cat1, cat2 ]


account1 =
    Persistence.Data.Account 43 "Account Name" <| Persistence.Data.AccountStart 10000 2023 3


account2 =
    Persistence.Data.Account 123 "Account Name 2" <| Persistence.Data.AccountStart 0 2023 3


entries : List BookEntry
entries =
    [ BookEntry "a" (Date.date 2023 3 1) "" 1000 account1 (Single cat1)
    , BookEntry "b" (Date.date 2023 3 2) "" 1000 account1 (Single cat2)
    , BookEntry "c" (Date.date 2023 4 12) "" 1000 account1 (Single cat2)
    , BookEntry "d" (Date.date 2023 4 28) "" 1000 account1 (Single cat2)
    , BookEntry "e" (Date.date 2023 5 4) "" 1000 account1 (Single cat1)
    , BookEntry "f" (Date.date 2023 3 4) "" 5000 account2 (Single cat1)
    ]


aggregate_book_entries : Test
aggregate_book_entries =
    describe "Parse categorization strings"
        [ test "aggregates by month" <|
            \_ ->
                (aggregate entries account1).rows
                    |> Expect.equal
                        [ MonthAggregate "March 2023" <| Dict.fromList [ ( 1, 1000 ), ( 2, 1000 ) ]
                        , MonthAggregate "April 2023" <| Dict.fromList [ ( 2, 2000 ) ]
                        , MonthAggregate "May 2023" <| Dict.fromList [ ( 1, 1000 ) ]
                        ]
        , test "filters by account" <|
            \_ ->
                (aggregate entries account2).rows
                    |> Expect.equal [ { month = "March 2023", entries = Dict.fromList [ ( 1, 5000 ) ] } ]
        ]
