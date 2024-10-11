module Processing.Aggregator exposing (AggregationType(..), Aggregator, all, fromAccount, fromAggregationGroup, fromCategories, fromCategory, fromCategoryGroup, uncategorized)

import Maybe.Extra
import Persistence.Account exposing (Account)
import Persistence.AggregationGroups exposing (AggregationGroup)
import Persistence.Category as CategoryGroup exposing (Category, CategoryGroup(..))
import Processing.BookEntry exposing (BookEntry, Categorization(..))


type alias Aggregator =
    { title : String
    , aggregationType : AggregationType
    , amount : BookEntry -> Int
    , runningSum : Maybe Int
    }


type AggregationType
    = Balance
    | Diff
    | Income
    | Expense
    | Other String


all : Maybe Int -> String -> Aggregator
all runningSum title =
    { title = title
    , aggregationType =
        if Maybe.Extra.isJust runningSum then
            Balance

        else
            Diff
    , amount = \e -> e.amount
    , runningSum = runningSum
    }


typeFromCategoryGroup : CategoryGroup -> AggregationType
typeFromCategoryGroup categoryGroup =
    case categoryGroup of
        CategoryGroup.Income ->
            Income

        CategoryGroup.Expense ->
            Expense

        CategoryGroup.Internal ->
            Other "Internal"


fromCategoryGroup : CategoryGroup -> Aggregator
fromCategoryGroup group =
    { title =
        case group of
            CategoryGroup.Income ->
                "Income"

            CategoryGroup.Expense ->
                "Expenses"

            CategoryGroup.Internal ->
                "Internal"
    , aggregationType = typeFromCategoryGroup group
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
    , runningSum = Nothing
    }


fromAggregationGroup : AggregationGroup -> Aggregator
fromAggregationGroup group =
    fromCategories group.name Nothing group.categories


fromCategories : String -> Maybe AggregationType -> List Int -> Aggregator
fromCategories title aggregationType categories =
    let
        isAggregated : Category -> Bool
        isAggregated cat =
            categories |> List.filter (\i -> cat.id == i) |> List.isEmpty |> not
    in
    { title = title
    , aggregationType = Maybe.withDefault (Other "Custom") aggregationType
    , amount =
        \bookEntry ->
            case bookEntry.categorization of
                None ->
                    0

                Single cat ->
                    if isAggregated cat then
                        bookEntry.amount

                    else
                        0

                Split entries ->
                    entries
                        |> List.filter (\e -> isAggregated e.category)
                        |> List.map .amount
                        |> List.sum
    , runningSum = Nothing
    }


fromCategory : Category -> Aggregator
fromCategory category =
    fromCategories category.name (Just <| typeFromCategoryGroup category.group) [ category.id ]


uncategorized : Aggregator
uncategorized =
    { title = "Uncategorized"
    , aggregationType = Other "Uncategorized"
    , amount =
        \bookEntry ->
            case bookEntry.categorization of
                None ->
                    bookEntry.amount

                _ ->
                    0
    , runningSum = Nothing
    }


fromAccount : Account -> Aggregator
fromAccount account =
    { title = account.name
    , aggregationType = Other "Account"
    , amount =
        \bookEntry ->
            if bookEntry.account == account then
                bookEntry.amount

            else
                0
    , runningSum = Just account.start.amount
    }
