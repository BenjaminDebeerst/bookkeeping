module Pages.Monthly exposing (Model, Msg, page)

import Components.Filter as Filter exposing (toAggregateFilter)
import Components.Table as T
import Components.Tabs as Tabs
import Config exposing (size)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, column, indexedTable, minimum, shrink, spacing, width)
import Layouts
import Page exposing (Page)
import Persistence.Account exposing (Account, Accounts)
import Persistence.Category as Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Persistence.RawEntry exposing (RawEntry)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate, startDate, startingBalances)
import Processing.Aggregator as Aggregator exposing (Aggregator)
import Processing.Filter as Filter exposing (EntryFilter)
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
            case shared of
                None ->
                    init [] []

                Loaded data ->
                    init (Dict.values data.accounts) (Dict.values data.rawEntries)

                Problem _ ->
                    init [] []
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


init : List Account -> List RawEntry -> () -> ( Model, Effect Msg )
init accounts entries _ =
    ( { filters = Filter.init accounts [] (List.map .date entries) Filter
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
        [ viewFilters model (Dict.values data.accounts)
        , viewTabs data model
        ]


viewTabs : Data -> Model -> Element Msg
viewTabs data model =
    Tabs.tabbedContent
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

        -- This is a bit special: The date filtering is not applied here, because the
        -- book entries prior to the selected range are required to calculate the
        -- starting sums for running-sum aggregators
        entryFilters : List EntryFilter
        entryFilters =
            [ model.filters.accounts |> List.map Filter.filterAccount |> Filter.any ]

        -- We discard out-of-range dates only after aggregation
        aggregatedData : Aggregate
        aggregatedData =
            getEntries data entryFilters dateAsc
                |> aggregate start startSums aggregators
                |> toAggregateFilter model.filters
    in
    showAggResults aggregatedData


viewFilters : Model -> List Account -> Element Msg
viewFilters model accounts =
    column [ spacing size.m, width (shrink |> minimum 800) ]
        [ Filter.accountFilter accounts model.filters
        , Filter.dateRangeFilter model.filters
        ]


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
