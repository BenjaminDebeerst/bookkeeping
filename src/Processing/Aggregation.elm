module Processing.Aggregation exposing (CellValue, MonthAggregate, aggregate, startDate)

import List.Extra
import Maybe.Extra
import Persistence.Account exposing (Account)
import Processing.Aggregator exposing (AggregationType, Aggregator)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Util.List exposing (partitionWith)
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias MonthAggregate =
    { month : YearMonth
    , values : List CellValue
    }


type alias CellValue =
    { label : String
    , aggregationType : AggregationType
    , value : Int
    }


startDate : List Account -> YearMonth
startDate accounts =
    accounts
        |> List.map (.start >> .yearMonth)
        |> List.Extra.minimumWith YearMonth.compare
        |> Maybe.withDefault YearMonth.zero


aggregate : YearMonth -> List Aggregator -> List BookEntry -> List MonthAggregate
aggregate month aggregators bookEntries =
    case bookEntries of
        [] ->
            []

        nonEmpty ->
            let
                ( rest, currentMonth, earlier ) =
                    nonEmpty |> partitionWith (.date >> YearMonth.fromDate >> YearMonth.compare month)

                currentMonthResults : List CellValue
                currentMonthResults =
                    List.map (aggregateOne currentMonth) aggregators

                carryOverAggregators : List Aggregator
                carryOverAggregators =
                    List.map2 carryOver currentMonthResults aggregators
            in
            [ MonthAggregate month currentMonthResults ]
                ++ aggregate (YearMonth.add 1 month) carryOverAggregators rest


carryOver : CellValue -> Aggregator -> Aggregator
carryOver result aggregator =
    { aggregator | runningSum = Maybe.Extra.next aggregator.runningSum (Just result.value) }


aggregateOne : List BookEntry -> Aggregator -> CellValue
aggregateOne bookEntries aggregator =
    CellValue
        aggregator.title
        aggregator.aggregationType
        (List.foldl
            (\entry sum -> sum + aggregator.amount entry)
            (aggregator.runningSum |> Maybe.withDefault 0)
            bookEntries
        )
