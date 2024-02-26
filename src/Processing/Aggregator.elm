module Processing.Aggregator exposing (Aggregator, all, fromCategory)

import Persistence.Category exposing (Category)
import Processing.BookEntry exposing (BookEntry, Categorization(..))


type alias Aggregator =
    { title : String
    , amount : BookEntry -> Int
    , runningSum : Bool
    }


all : Bool -> String -> Aggregator
all runningSum title =
    { title = title
    , amount = \e -> e.amount
    , runningSum = runningSum
    }


fromCategory : Category -> Aggregator
fromCategory category =
    { title = category.name
    , amount =
        \bookEntry ->
            case bookEntry.categorization of
                None ->
                    0

                Single cat ->
                    if category.id == cat.id then
                        bookEntry.amount

                    else
                        0

                Split entries ->
                    entries
                        |> List.filter (\e -> e.category.id == category.id)
                        |> List.map .amount
                        |> List.sum
    , runningSum = False
    }
