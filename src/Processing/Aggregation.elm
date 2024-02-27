module Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate, startDate, startingBalances)

import Dict exposing (Dict)
import List.Extra
import Persistence.Account exposing (Account)
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


startingBalances : List Account -> Dict String Int
startingBalances accounts =
    Dict.fromList
        ([]
            ++ (accounts |> List.map (\a -> ( a.name, a.start.amount )))
            ++ (accounts |> List.map (.start >> .amount) |> List.sum |> (\s -> [ ( "Balance", s ) ]))
        )


startDate : List Account -> Date
startDate accounts =
    accounts
        |> List.map (\a -> Date.date a.start.year a.start.month 1)
        |> List.Extra.minimumWith Date.compare
        |> Maybe.withDefault (Date.date 2000 1 1)


aggregate : Date -> Dict String Int -> List Aggregator -> List BookEntry -> Aggregate
aggregate start initialSums aggregators bookEntries =
    Aggregate (List.map .title aggregators)
        (aggregateMonthly start initialSums aggregators bookEntries)


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
