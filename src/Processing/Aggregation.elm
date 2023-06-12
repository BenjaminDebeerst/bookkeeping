module Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)

import Dict exposing (Dict)
import List.Extra
import Persistence.Data exposing (Account, AccountStart, Category, Data, RawEntry)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Time.Date as Date exposing (Date, date)


type alias MonthAggregate =
    { month : String
    , balance : Int
    , entries : Dict Int Int -- Category id -> Amount
    }


type alias Aggregate =
    { accounts : List Account
    , rows : List MonthAggregate
    }


{-| Book entries MUST be given in date ascending order. Entries showing up out of date order may be ignored in the computation.
-}
aggregate : List BookEntry -> Aggregate
aggregate bookEntries =
    let
        accounts =
            bookEntries
                |> List.map .account
                |> List.Extra.unique

        startMonth =
            accounts
                |> List.map (.start >> (\s -> Date.date s.year s.month 1))
                |> List.sortBy Date.toTuple
                |> List.head

        -- There's a subtle detail here, namely that this sum ignores the start date of all accounts, adding their
        -- start balance to the very beginning of what is currently filtered. It only affects the running monthly
        -- balance though.
        -- Otherwise there would need to be some kind of "phantom book entry" for the addition of an account.
        startBalance =
            accounts |> List.map (.start >> .amount) |> List.sum
    in
    case startMonth of
        Just date ->
            Aggregate accounts (aggHelper date ( Dict.empty, startBalance ) bookEntries [])

        Nothing ->
            Aggregate accounts []


aggHelper : Date -> ( Dict Int Int, Int ) -> List BookEntry -> List MonthAggregate -> List MonthAggregate
aggHelper currentMonth ( monthEntries, balance ) remainingBookEntries aggregatedMonths =
    case remainingBookEntries of
        [] ->
            aggregatedMonths ++ [ MonthAggregate (monthString currentMonth) balance monthEntries ]

        be :: tail ->
            case compare currentMonth be of
                GT ->
                    -- This is a book entry before the start of the accounts or the data was not ordered.
                    -- Skip the value and carry on
                    aggHelper
                        currentMonth
                        ( monthEntries, balance )
                        tail
                        aggregatedMonths

                EQ ->
                    aggHelper
                        currentMonth
                        ( addCategorizedAmount be monthEntries, balance + be.amount )
                        tail
                        aggregatedMonths

                LT ->
                    aggHelper
                        (Date.addMonths 1 currentMonth)
                        ( Dict.empty, balance )
                        remainingBookEntries
                        (aggregatedMonths ++ [ MonthAggregate (monthString currentMonth) balance monthEntries ])


monthString : Date -> String
monthString date =
    String.join " "
        [ case Date.month date of
            1 ->
                "January"

            2 ->
                "February"

            3 ->
                "March"

            4 ->
                "April"

            5 ->
                "May"

            6 ->
                "June"

            7 ->
                "July"

            8 ->
                "August"

            9 ->
                "September"

            10 ->
                "October"

            11 ->
                "November"

            12 ->
                "December"

            i ->
                "Month " ++ String.fromInt i
        , String.fromInt <| Date.year date
        ]


addCategorizedAmount : BookEntry -> Dict Int Int -> Dict Int Int
addCategorizedAmount bookEntry categoryAmounts =
    case bookEntry.categorization of
        None ->
            categoryAmounts

        Single cat ->
            addToAmounts cat.id bookEntry.amount categoryAmounts

        Split entries ->
            List.foldl
                (\entrySplit amounts ->
                    addToAmounts entrySplit.category.id entrySplit.amount amounts
                )
                categoryAmounts
                entries


addToAmounts : Int -> Int -> Dict Int Int -> Dict Int Int
addToAmounts id amount amounts =
    Dict.update id
        (\a ->
            case a of
                Just n ->
                    Just (n + amount)

                Nothing ->
                    Just amount
        )
        amounts


yearMonth : Date -> Int
yearMonth date =
    Date.year date * 100 + Date.month date


compare : Date -> BookEntry -> Basics.Order
compare date bookEntry =
    Basics.compare (yearMonth date) (yearMonth bookEntry.date)
