module Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate, startDate, startingBalances)

import Dict exposing (Dict)
import List.Extra
import Persistence.Account exposing (Account)
import Processing.Aggregator exposing (Aggregator)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Util.List exposing (partitionWith)
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias MonthAggregate =
    { month : YearMonth
    , columns : Dict String Int -- title -> Amount}
    }


type alias Aggregate =
    { aggregators : List String
    , rows : List MonthAggregate
    }


startingBalances : List Account -> Dict String Int
startingBalances accounts =
    Dict.fromList
        ([]
            ++ (accounts |> List.map (\a -> ( a.name, a.start.amount )))
            ++ (accounts |> List.map (.start >> .amount) |> List.sum |> (\s -> [ ( "Month-End Balance", s ) ]))
        )


startDate : List Account -> YearMonth
startDate accounts =
    accounts
        |> List.map (.start >> .yearMonth)
        |> List.Extra.minimumWith YearMonth.compare
        |> Maybe.withDefault YearMonth.zero


aggregate : YearMonth -> Dict String Int -> List Aggregator -> List BookEntry -> Aggregate
aggregate start initialSums aggregators bookEntries =
    Aggregate (List.map .title aggregators)
        (aggregateMonthly start initialSums aggregators bookEntries)


aggregateMonthly : YearMonth -> Dict String Int -> List Aggregator -> List BookEntry -> List MonthAggregate
aggregateMonthly month initialSums aggregators bookEntries =
    case bookEntries of
        [] ->
            []

        nonEmpty ->
            let
                ( rest, currentMonth, earlier ) =
                    nonEmpty |> partitionWith (.date >> YearMonth.fromDate >> YearMonth.compare month)

                currentMonthResults =
                    aggregateAllWith aggregators currentMonth initialSums
            in
            [ MonthAggregate month currentMonthResults ]
                ++ aggregateMonthly (YearMonth.add 1 month) currentMonthResults aggregators rest


aggregateAllWith : List Aggregator -> List BookEntry -> Dict String Int -> Dict String Int
aggregateAllWith aggregators bookEntries startingSums =
    let
        filteredStartingSums =
            aggregators
                |> List.filterMap
                    (\a ->
                        if a.runningSum then
                            Dict.get a.title startingSums |> Maybe.map (\sum -> ( a.title, sum ))

                        else
                            Nothing
                    )
                |> Dict.fromList
    in
    bookEntries |> List.foldl (addToDict aggregators) filteredStartingSums


addToDict : List Aggregator -> BookEntry -> Dict String Int -> Dict String Int
addToDict aggregators entry amounts =
    aggregators |> List.foldl (aggregateEntry entry) amounts


aggregateEntry : BookEntry -> Aggregator -> Dict String Int -> Dict String Int
aggregateEntry bookEntry aggregator amounts =
    amounts
        |> Dict.update aggregator.title
            (\a ->
                case a of
                    Just n ->
                        Just (n + aggregator.amount bookEntry)

                    Nothing ->
                        Just (aggregator.amount bookEntry)
            )
