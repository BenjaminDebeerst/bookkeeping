module Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)

import Dict exposing (Dict)
import Persistence.Data exposing (Account, AccountStart, Category, Data, RawEntry)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Processing.Filter exposing (Filter, filterAccount)
import Time.Date as Date exposing (Date, date)


type alias MonthAggregate =
    { month : String
    , balance : Int
    , entries : Dict Int Int -- Category id -> Amount
    }


type alias Aggregate =
    { account : Account
    , rows : List MonthAggregate
    }


{-| Book entries need to be given in date ascending order
-}
aggregate : List BookEntry -> Account -> Aggregate
aggregate bookEntries account =
    let
        startMonth =
            date account.start.year account.start.month 1

        accEntries =
            bookEntries
                |> List.filter (filterAccount account)

        monthAggregates =
            aggHelper startMonth ( Dict.empty, account.start.amount ) accEntries []
    in
    Aggregate account monthAggregates


aggHelper : Date -> ( Dict Int Int, Int ) -> List BookEntry -> List MonthAggregate -> List MonthAggregate
aggHelper currentMonth ( monthEntries, balance ) remainingBookEntries aggregatedMonths =
    case remainingBookEntries of
        [] ->
            aggregatedMonths ++ [ MonthAggregate (monthString currentMonth) balance monthEntries ]

        be :: tail ->
            -- FIXME Ensure this is tail recursive. Otherwise the stack will explode.
            if equalMonths currentMonth be then
                aggHelper
                    currentMonth
                    ( addCategorizedAmount be monthEntries, balance + be.amount )
                    tail
                    aggregatedMonths

            else
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


equalMonths : Date -> BookEntry -> Bool
equalMonths date bookEntry =
    Date.year date
        == Date.year bookEntry.date
        && Date.month date
        == Date.month bookEntry.date
