module Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)

import Dict exposing (Dict)
import Processing.Aggregator exposing (Aggregator)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit)
import Time.Date as Date exposing (Date)
import Util.Date as Date
import Util.List exposing (partitionWith)


type alias MonthAggregate =
    { month : Date
    , columns : Dict String Int -- title -> Amount}
    }


type alias Aggregate =
    { aggregators : List String
    , rows : List MonthAggregate
    }


aggregate : Date -> Dict String Int -> List Aggregator -> List BookEntry -> Aggregate
aggregate startDate initialSums aggregators bookEntries =
    Aggregate (List.map .title aggregators)
        (aggregateMonthly startDate initialSums aggregators bookEntries)


aggregateMonthly : Date -> Dict String Int -> List Aggregator -> List BookEntry -> List MonthAggregate
aggregateMonthly month initialSums aggregators bookEntries =
    case bookEntries of
        [] ->
            []

        nonEmpty ->
            let
                ( earlier, currentMonth, rest ) =
                    nonEmpty |> partitionWith (\e -> Date.compareMonths e.date month)

                currentMonthResults =
                    aggregateAllWith aggregators currentMonth initialSums
            in
            [ MonthAggregate month currentMonthResults ]
                ++ aggregateMonthly (Date.addMonths 1 month) currentMonthResults aggregators rest


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
