module Aggregation exposing (..)

import Dict
import Expect exposing (Expectation)
import Persistence.Account exposing (Account, AccountStart, account)
import Persistence.Category exposing (Category, CategoryGroup(..), category)
import Processing.Aggregation exposing (MonthAggregate, aggregate)
import Processing.Aggregator exposing (Aggregator)
import Processing.BookEntry exposing (BookEntry, Categorization(..))
import Test exposing (..)
import Time.Date as Date


cat1 =
    category 1 "Cat 1" "CAT1" Expense []


cat2 =
    category 2 "Cat 2" "CAT2" Expense []


categories : List Category
categories =
    [ cat1, cat2 ]


account1 =
    account 1 "Account One" 10 2023 3


account2 =
    account 2 "Account Name 2" 20 2023 4


entries : List BookEntry
entries =
    [ BookEntry "a" (Date.date 2023 3 1) "" 2 account1 (Single cat1)
    , BookEntry "b" (Date.date 2023 3 2) "" 2 account1 (Single cat2)
    , BookEntry "c" (Date.date 2023 4 4) "" 2 account2 (Single cat1)
    , BookEntry "d" (Date.date 2023 4 6) "" 2 account1 (Single cat2)
    , BookEntry "e" (Date.date 2023 4 9) "" 2 account1 (Single cat2)
    , BookEntry "f" (Date.date 2023 5 4) "" 2 account1 (Single cat1)
    ]


aggregate_book_entries : Test
aggregate_book_entries =
    describe "Running an aggregate"
        [ test "aggregates non-running sums, ignoring start values" <|
            \_ ->
                (aggregate
                    (Date.date 2023 2 1)
                    (Dict.fromList [ ( "Sum", 10 ) ])
                    [ Aggregator "Sum" .amount False ]
                    entries
                ).rows
                    -- Expecting here:
                    -- - list starts at start date
                    -- - sum is exactly sum of entries of list above for a given month
                    -- - if there is no entry for a given month, the value is absent
                    -- - list ends where the entries above end
                    |> Expect.equal
                        [ { month = Date.date 2023 2 1, columns = Dict.empty }
                        , { month = Date.date 2023 3 1, columns = Dict.fromList [ ( "Sum", 4 ) ] }
                        , { month = Date.date 2023 4 1, columns = Dict.fromList [ ( "Sum", 6 ) ] }
                        , { month = Date.date 2023 5 1, columns = Dict.fromList [ ( "Sum", 2 ) ] }
                        ]
        , test " aggregates for running sums, using the start value" <|
            \_ ->
                (aggregate
                    (Date.date 2023 2 1)
                    (Dict.fromList [ ( "Cumulative", 10 ) ])
                    [ Aggregator "Cumulative" .amount True ]
                    entries
                ).rows
                    -- Expecting here:
                    -- - Monthly sums are carried over to next month
                    -- - starting sum is included in the sums
                    |> Expect.equal
                        [ { month = Date.date 2023 2 1, columns = Dict.fromList [ ( "Cumulative", 10 ) ] }
                        , { month = Date.date 2023 3 1, columns = Dict.fromList [ ( "Cumulative", 14 ) ] }
                        , { month = Date.date 2023 4 1, columns = Dict.fromList [ ( "Cumulative", 20 ) ] }
                        , { month = Date.date 2023 5 1, columns = Dict.fromList [ ( "Cumulative", 22 ) ] }
                        ]
        ]
