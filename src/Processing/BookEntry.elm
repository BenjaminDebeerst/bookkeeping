module Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)

import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Persistence.RawEntry as RawEntry
import Time.Date exposing (Date)


type alias BookEntry =
    { id : Int
    , date : Date
    , description : String
    , amount : Int
    , account : Account
    , categorization : Categorization
    }


type Categorization
    = None
    | Single Category
    | Split (List EntrySplit)


type alias EntrySplit =
    { category : Category
    , amount : Int
    }


toPersistence : Categorization -> Maybe RawEntry.Categorization
toPersistence cat =
    case cat of
        None ->
            Nothing

        Single c ->
            Just (RawEntry.Single c.id)

        Split list ->
            list |> List.map (\se -> RawEntry.SplitCatEntry se.category.id se.amount) |> RawEntry.Split |> Just
