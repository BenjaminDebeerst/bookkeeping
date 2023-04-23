module Pages.Bookings exposing (Model, Msg, page)

import Array
import Html exposing (Html, div, td, text, tr)
import Html.Attributes exposing (id)
import Maybe.Extra
import Navigation
import Page
import Request exposing (Request)
import Shared
import Storage exposing (CsvLine, Storage)
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = Noop


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model
    , Cmd.none
    )



-- VIEW


view : Storage -> Model -> View Msg
view storage _ =
    { title = "Book"
    , body = [ showData storage ]
    }


showData : Storage -> Html Msg
showData storage =
    div []
        [ Navigation.view
        , div [ id "data" ]
            [ div [ id "ledger" ] (viewTable <| tableFromCsvData storage.rawData)
            ]
        ]


simpleCsvData : List CsvLine -> List (List String)
simpleCsvData l =
    l |> List.map (\t -> [ Tuple.first t, Tuple.second t ])


tableFromCsvData : List CsvLine -> List (List String)
tableFromCsvData l =
    l
        |> List.map parseCsvLine
        |> Maybe.Extra.values
        |> List.map (\e -> [ e.id, e.date, String.fromInt e.amount, e.description ])



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


type alias Entry =
    { id : String
    , date : String
    , description : String
    , amount : Int
    }


parseCsvLine : Storage.CsvLine -> Maybe Entry
parseCsvLine line =
    let
        cells =
            Tuple.second line |> String.split ";" |> List.map String.trim |> Array.fromList

        date =
            get cells 4

        descr =
            [ get cells 6, get cells 9, get cells 10 ] |> List.intersperse " " |> String.concat

        r =
            String.replace

        amount =
            String.toInt <| r "," "" <| r "." "" <| r " " "" <| get cells 11
    in
    Maybe.map (\a -> { id = Tuple.first line, date = date, description = descr, amount = a }) amount


get a i =
    Maybe.withDefault "X" <| Array.get i a


viewTable : List (List String) -> List (Html Msg)
viewTable t =
    t |> List.map (\r -> tr [] (r |> List.map (\c -> td [] [ text c ])))
