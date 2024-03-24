module Pages.Monthly exposing (Model, Msg, page)

import Components.Filter as Filter
import Components.Table as T
import Components.Tabs as Tabs
import Config exposing (size)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, column, indexedTable, spacing)
import Layouts
import Page exposing (Page)
import Persistence.Account exposing (Account, Accounts)
import Persistence.Category as Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate, startDate, startingBalances)
import Processing.Aggregator as Aggregator exposing (Aggregator)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Shared.Model exposing (Model(..))
import Util.Formats exposing (formatEuro, formatYearMonth)
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
        |> Page.withLayout (\_ -> Layouts.Sidebar { dataSummary = dataSummary shared })



-- INIT


type Tab
    = ByCategory
    | ByAccount
    | Overview


type alias Model =
    { filters : Filter.Model Msg
    , tab : Tab
    }


init : List Account -> () -> ( Model, Effect Msg )
init accounts _ =
    ( { filters = Filter.init accounts [] Filter
      , tab = Overview
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = Filter Filter.Msg
    | TabSelection Tab


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Filter filterMsg ->
            let
                ( filters, effect ) =
                    Filter.update filterMsg model.filters
            in
            ( { model | filters = filters }, effect )

        TabSelection tab ->
            ( { model | tab = tab }, Effect.none )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    column [ spacing size.m ]
        [ Dict.values data.accounts |> showFilters model
        , Tabs.tabbedContent
            { allTabs = [ Overview, ByCategory, ByAccount ]
            , selectedTab = model.tab
            , tabTitles =
                \tab ->
                    case tab of
                        ByCategory ->
                            "By Category"

                        ByAccount ->
                            "By Account"

                        Overview ->
                            "Overview"
            , tabMsg = TabSelection
            , content =
                case model.tab of
                    ByCategory ->
                        byCategory data model

                    ByAccount ->
                        byAccount data model

                    Overview ->
                        overview data model
            }
        ]


overview : Data -> Model -> Element Msg
overview data model =
    showAggregations
        data
        model
        [ Aggregator.all True "Balance"
        , Aggregator.all False "Sum"
        , Aggregator.fromCategoryGroup Income
        , Aggregator.fromCategoryGroup Expense
        , Aggregator.fromCategoryGroup Internal
        , Aggregator.uncategorized
        ]


byCategory : Data -> Model -> Element Msg
byCategory data model =
    showAggregations
        data
        model
        ([ Aggregator.all True "Balance"
         , Aggregator.all False "Diff"
         ]
            ++ (Dict.values data.categories |> List.sortWith Category.order |> List.map Aggregator.fromCategory)
        )


byAccount : Data -> Model -> Element Msg
byAccount data model =
    showAggregations
        data
        model
        ([ Aggregator.all True "Balance"
         , Aggregator.all False "Sum"
         ]
            ++ (model.filters.accounts |> List.sortBy .name |> List.map (Aggregator.fromAccount True))
        )



-- AGGREGATION TABLE


showAggregations : Data -> Model -> List Aggregator -> Element Msg
showAggregations data model aggregators =
    let
        start =
            startDate model.filters.accounts

        startSums =
            startingBalances model.filters.accounts

        entryFilters =
            Filter.toFilter model.filters

        aggregatedData =
            getEntries data entryFilters dateAsc
                |> aggregate start startSums aggregators
    in
    showAggResults aggregatedData


showFilters : Model -> List Account -> Element Msg
showFilters model accounts =
    Filter.accountFilter accounts model.filters


showAggResults : Aggregate -> Element msg
showAggResults aggregation =
    indexedTable T.tableStyle
        { data = aggregation.rows
        , columns = T.textColumn "Month" (.month >> formatYearMonth) :: aggregationColumns aggregation.aggregators
        }


aggregationColumns : List String -> List (IndexedColumn MonthAggregate msg)
aggregationColumns aggregationNames =
    aggregationNames
        |> List.map
            (\agg ->
                T.styledColumn agg (.columns >> Dict.get agg >> Maybe.withDefault 0 >> formatEuro)
            )
