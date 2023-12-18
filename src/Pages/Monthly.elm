module Pages.Monthly exposing (Model, Msg, page)

import Components.Filter as Filter
import Components.Layout as Layout exposing (formatEuro, size, style, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Dict exposing (Dict)
import Element exposing (Element, IndexedColumn, column, el, indexedTable, spacing, text)
import Gen.Params.Monthly exposing (Params)
import Page
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Request
import Shared exposing (Model(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init =
            init
                (case shared of
                    None ->
                        []

                    Loaded data ->
                        Dict.values data.accounts

                    Problem _ ->
                        []
                )
        , update = updateOrRedirectOnError shared req update
        , view = viewDataOnly shared view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    { filters : Filter.Model
    }


init : List Account -> ( Model, Cmd Msg )
init accounts =
    ( { filters = Filter.init accounts }, Cmd.none )



-- UPDATE


type Msg
    = Filter Filter.Msg


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Filter filterMsg ->
            ( { model | filters = Filter.update filterMsg model.filters }, Cmd.none )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    let
        filter =
            Filter.toFilter (Dict.values data.categories) model.filters

        aggregatedData =
            aggregate <| getEntries data filter dateAsc
    in
    Layout.page "Monthly" <|
        [ showFilters model <| Dict.values data.accounts
        , showData data aggregatedData
        ]


showFilters : Model -> List Account -> Element Msg
showFilters model accounts =
    column [ spacing size.s ]
        [ el style.h2 <| text "Filters"
        , Filter.accountFilter accounts model.filters Filter
        ]


showData : Data -> Aggregate -> Element msg
showData data aggregate =
    indexedTable T.tableStyle
        { data = aggregate.rows
        , columns =
            [ T.textColumn "Month" .month
            , T.styledColumn "Balance" (.balance >> formatEuro)
            , T.styledColumn "Diff" (.diff >> formatEuro)
            ]
                ++ categoryColumns data.categories
        }


categoryColumns : Dict Int Category -> List (IndexedColumn MonthAggregate msg)
categoryColumns categories =
    Dict.values categories
        |> List.sortBy categoryOrder
        |> List.map
            (\cat ->
                T.styledColumn cat.name (.entries >> Dict.get cat.id >> Maybe.withDefault 0 >> formatEuro)
            )


categoryOrder : Category -> String
categoryOrder category =
    case category.group of
        Income ->
            "0" ++ category.name

        Expense ->
            "1" ++ category.name

        Internal ->
            "2" ++ category.name
