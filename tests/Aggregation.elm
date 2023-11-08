module Aggregation exposing (..)

import Dict
import Expect exposing (Expectation)
import Persistence.Account exposing (Account, AccountStart)
import Persistence.Category exposing (Category)
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
    Account 1 "Account Name" <| AccountStart 10 2023 3


account2 =
    Account 2 "Account Name 2" <| AccountStart 20 2023 4


entries : List BookEntry
entries =
    [ BookEntry "a" (Date.date 2023 3 1) "" 1 account1 (Single cat1)
    , BookEntry "b" (Date.date 2023 3 2) "" 1 account1 (Single cat2)
    , BookEntry "c" (Date.date 2023 4 4) "" 5 account2 (Single cat1)
    , BookEntry "d" (Date.date 2023 4 12) "" 1 account1 (Single cat2)
    , BookEntry "e" (Date.date 2023 4 28) "" 1 account1 (Single cat2)
    , BookEntry "f" (Date.date 2023 5 4) "" 1 account1 (Single cat1)
    ]


aggregate_book_entries : Test
aggregate_book_entries =
    describe "Entry aggregation"
        [ test "aggregates by month for single account" <|
            \_ ->
                (aggregate (List.filter (\be -> be.account.id == 1) entries)).rows
                    |> Expect.equal
                        [ { month = "March 2023", balance = 12, entries = Dict.fromList [ ( 1, 1 ), ( 2, 1 ) ] }
                        , { month = "April 2023", balance = 14, entries = Dict.fromList [ ( 2, 2 ) ] }
                        , { month = "May 2023", balance = 15, entries = Dict.fromList [ ( 1, 1 ) ] }
                        ]
        , test "filters by month for multiple accounts" <|
            \_ ->
                (aggregate entries).rows
                    |> Expect.equal
                        -- Yes, although account 2 starts only in April, its starting balance is added to March as well
                        [ { month = "March 2023", balance = 32, entries = Dict.fromList [ ( 1, 1 ), ( 2, 1 ) ] }
                        , { month = "April 2023", balance = 39, entries = Dict.fromList [ ( 1, 5 ), ( 2, 2 ) ] }
                        , { month = "May 2023", balance = 40, entries = Dict.fromList [ ( 1, 1 ) ] }
                        ]
        ]
