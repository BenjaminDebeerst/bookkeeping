module Processing.Aggregator exposing (Aggregator, all, fromAccount, fromCategory, fromCategoryGroup, uncategorized)

import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category, CategoryGroup(..))
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


fromCategoryGroup : CategoryGroup -> Aggregator
fromCategoryGroup group =
    { title =
        case group of
            Income ->
                "Income"

            Expense ->
                "Expenses"

            Internal ->
                "Internal"
    , amount =
        \bookEntry ->
            case bookEntry.categorization of
                None ->
                    0

                Single cat ->
                    if group == cat.group then
                        bookEntry.amount

                    else
                        0

                Split entries ->
                    entries
                        |> List.filter (\e -> e.category.group == group)
                        |> List.map .amount
                        |> List.sum
    , runningSum = False
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


uncategorized : Aggregator
uncategorized =
    { title = "Uncategorized"
    , amount =
        \bookEntry ->
            case bookEntry.categorization of
                None ->
                    bookEntry.amount

                _ ->
                    0
    , runningSum = False
    }


fromAccount : Bool -> Account -> Aggregator
fromAccount runningSum account =
    { title = account.name
    , amount =
        \bookEntry ->
            if bookEntry.account == account then
                bookEntry.amount

            else
                0
    , runningSum = runningSum
    }
