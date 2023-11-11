module Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)

import Persistence.Data as Data exposing (Account, Category)
import Time.Date exposing (Date)


type alias BookEntry =
    { id : String
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


toPersistence : Categorization -> Maybe Data.Categorization
toPersistence cat =
    case cat of
        None ->
            Nothing

        Single c ->
            Just (Data.Single c.id)

        Split list ->
            list |> List.map (\se -> Data.SplitCatEntry se.category.id se.amount) |> Data.Split |> Just
