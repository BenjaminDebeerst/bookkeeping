module Pages.Monthly exposing (Model, Msg, page)

import Components.Filter as Filter
import Components.Table as T
import Config exposing (size, style)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, column, el, indexedTable, padding, spacing, text)
import Layouts
import Page exposing (Page)
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Route exposing (Route)
import Shared
import Shared.Model exposing (Model(..))
import Util.Formats exposing (formatEuro)
import Util.Layout exposing (dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
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
        , update = dataUpdate shared update
        , view = dataView shared "Monthly" view
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Sidebar {})



-- INIT


type alias Model =
    { filters : Filter.Model
    }


init : List Account -> () -> ( Model, Effect Msg )
init accounts _ =
    ( { filters = Filter.init accounts }, Effect.none )



-- UPDATE


type Msg
    = Filter Filter.Msg


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Filter filterMsg ->
            ( { model | filters = Filter.update filterMsg model.filters }, Effect.none )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    let
        filter =
            Filter.toFilter (Dict.values data.categories) model.filters

        aggregatedData =
            aggregate <| getEntries data filter dateAsc
    in
    column [ spacing size.m ]
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
