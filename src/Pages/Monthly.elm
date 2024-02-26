module Pages.Monthly exposing (Model, Msg, page)

import Components.Filter as Filter
import Components.Table as T
import Components.Tabs as Tabs
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, indexedTable)
import Layouts
import List.Extra
import Page exposing (Page)
import Persistence.Account exposing (Account)
import Persistence.Category as Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)
import Processing.Aggregator as Aggregator
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Shared.Model exposing (Model(..))
import Time.Date as Date exposing (Date)
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


type alias Model =
    { filters : Filter.Model
    , tab : Tab
    }


init : List Account -> () -> ( Model, Effect Msg )
init accounts _ =
    ( { filters = Filter.init accounts, tab = ByCategory }, Effect.none )



-- UPDATE


type Msg
    = Filter Filter.Msg
    | TabSelection Tab


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Filter filterMsg ->
            ( { model | filters = Filter.update filterMsg model.filters }, Effect.none )

        TabSelection tab ->
            ( { model | tab = tab }, Effect.none )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    Tabs.tabbedContent
        { allTabs = [ ByCategory, ByAccount ]
        , selectedTab = model.tab
        , tabTitles =
            \tab ->
                case tab of
                    ByCategory ->
                        "By Category"

                    ByAccount ->
                        "By Account"
        , tabMsg = TabSelection
        , content =
            case model.tab of
                ByCategory ->
                    viewByCategory data model

                ByAccount ->
                    viewByAccount data model
        }


showFilters : Model -> List Account -> Element Msg
showFilters model accounts =
    Filter.accountFilter accounts model.filters Filter



-- BY CATEGORY


viewByCategory : Data -> Model -> List (Element Msg)
viewByCategory data model =
    let
        aggregators =
            [ Aggregator.all True "Balance"
            , Aggregator.all False "Diff"
            ]
                ++ (Dict.values data.categories
                        |> List.sortWith Category.order
                        |> List.map Aggregator.fromCategory
                   )

        bookEntryFilter =
            Filter.toFilter (Dict.values data.categories) model.filters

        startDate =
            model.filters.accounts
                |> List.map (\a -> Date.date a.start.year a.start.month 1)
                |> List.Extra.minimumWith Date.compare
                |> Maybe.withDefault (Date.date 0 0 0)

        startSums =
            model.filters.accounts
                |> List.map (.start >> .amount)
                |> List.sum
                |> (\s -> Dict.fromList [ ( "Balance", s ) ])

        aggregatedData =
            aggregate startDate startSums aggregators <| getEntries data bookEntryFilter dateAsc
    in
    [ showFilters model <| Dict.values data.accounts
    , showAggResults aggregatedData
    ]



-- BY ACCOUNT


viewByAccount : Data -> Model -> List (Element Msg)
viewByAccount data model =
    let
        aggregators =
            [ Aggregator.all True "Balance"
            , Aggregator.all False "Sum"
            ]
                ++ (model.filters.accounts
                        |> List.sortBy .name
                        |> List.map (Aggregator.fromAccount True)
                   )

        startDate =
            model.filters.accounts
                |> List.map (\a -> Date.date a.start.year a.start.month 1)
                |> List.Extra.minimumWith Date.compare
                |> Maybe.withDefault (Date.date 0 0 0)

        startSums =
            ((model.filters.accounts
                |> List.map (\a -> ( a.name, a.start.amount ))
             )
                ++ (model.filters.accounts |> List.map (.start >> .amount) |> List.sum |> (\s -> [ ( "Balance", s ) ]))
            )
                |> Dict.fromList

        aggregatedData =
            aggregate startDate startSums aggregators <| getEntries data [] dateAsc
    in
    [ showFilters model <| Dict.values data.accounts
    , showAggResults aggregatedData
    ]



-- AGGREGATION TABLE


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
