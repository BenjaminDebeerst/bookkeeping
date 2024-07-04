module Pages.Monthly exposing (Model, Msg, page)

import Components.Filter as Filter exposing (toAggregateFilter)
import Components.Table as T
import Components.Tabs as Tabs
import Config exposing (size, style)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, column, indexedTable, minimum, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (labelHidden)
import Layouts
import List.Extra
import Maybe.Extra
import Page exposing (Page)
import Persistence.Account exposing (Account, Accounts)
import Persistence.Audits as Audits exposing (Audits)
import Persistence.Category as Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Persistence.RawEntry exposing (RawEntry)
import Persistence.Storage exposing (updateAudits)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate, startDate, startingBalances)
import Processing.Aggregator as Aggregator exposing (Aggregator)
import Processing.Filter as Filter exposing (EntryFilter)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Util.Formats exposing (formatEuro, formatYearMonth)
import Util.Layout exposing (dataInit, dataUpdate, dataView)
import Util.YearMonth exposing (YearMonth)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> dataInit shared (init [] []) initFromData
        , update = dataUpdate shared update
        , view = dataView shared "Monthly" view
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Tabs { dataSummary = dataSummary shared })



-- INIT


type Tab
    = ByCategory
    | ByAccount
    | Overview


type alias Model =
    { filters : Filter.Model Msg
    , tab : Tab
    , edits : Maybe (List ( YearMonth, String ))
    }


initFromData : Data -> Model
initFromData data =
    init (Dict.values data.accounts) (Dict.values data.rawEntries.entries)


init : List Account -> List RawEntry -> Model
init accounts entries =
    { filters = Filter.init accounts [] (List.map .date entries) Filter
    , tab = Overview
    , edits = Nothing
    }



-- UPDATE


type Msg
    = Filter Filter.Msg
    | TabSelection Tab
    | StartEdit
    | Save
    | Abort
    | EditComment YearMonth String


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

        StartEdit ->
            ( { model | edits = Just [] }, Effect.none )

        EditComment ym comment ->
            case model.edits of
                Nothing ->
                    ( model, Effect.none )

                Just edits ->
                    let
                        edited : List ( YearMonth, String )
                        edited =
                            case List.Extra.findIndex (\( ym2, _ ) -> ym == ym2) edits of
                                Just i ->
                                    List.Extra.setAt i ( ym, comment ) edits

                                Nothing ->
                                    ( ym, comment ) :: edits
                    in
                    ( { model | edits = Just edited }, Effect.none )

        Save ->
            let
                acc : ( YearMonth, String ) -> Audits -> Audits
                acc ( ym, comment ) =
                    Audits.update ym (\a -> { a | comment = comment })

                updated =
                    model.edits
                        |> Maybe.withDefault []
                        |> List.foldl acc data.audits
            in
            ( { model | edits = Nothing }, updateAudits updated data |> Effect.store )

        Abort ->
            ( { model | edits = Nothing }, Effect.none )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    column [ spacing size.m ]
        [ viewFilters model (Dict.values data.accounts)
        , viewActions model
        , viewTabs data model
        ]


viewActions : Model -> Element Msg
viewActions model =
    if model.edits |> Maybe.Extra.isJust then
        Element.row [ spacing size.m ]
            [ Input.button style.button { onPress = Just Save, label = text "Save" }
            , Input.button style.button { onPress = Just Abort, label = text "Abort" }
            ]

    else
        Input.button style.button { onPress = Just StartEdit, label = text "Edit" }


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
        , rightCorner = []
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
    showAggResults data.audits model.edits aggregatedData


viewFilters : Model -> List Account -> Element Msg
viewFilters model accounts =
    column [ spacing size.m, width (shrink |> minimum 800) ]
        [ Filter.accountFilter accounts model.filters
        , Filter.dateRangeFilter model.filters
        ]


showAggResults : Audits -> Maybe (List ( YearMonth, String )) -> Aggregate -> Element Msg
showAggResults audits edits aggregation =
    indexedTable T.tableStyle
        { data = aggregation.rows
        , columns =
            T.textColumn "Month" (.month >> formatYearMonth)
                :: aggregationColumns aggregation.aggregators
                ++ [ T.styledColumn "Comment" (.month >> commentCell audits edits) ]
        }


commentCell : Audits -> Maybe (List ( YearMonth, String )) -> YearMonth -> Element Msg
commentCell audits edits yearMonth =
    let
        editing =
            Maybe.Extra.isJust edits

        findYM =
            \( ym, c ) ->
                if ym == yearMonth then
                    Just c

                else
                    Nothing

        comment =
            edits
                |> Maybe.andThen (List.Extra.findMap findYM)
                |> Maybe.withDefault (Audits.get yearMonth audits |> .comment)
    in
    if editing then
        Input.text
            [ Element.padding 1
            , Border.rounded 3
            , Background.color (Element.rgb 1 1 1)
            , Border.width 0
            , Element.spacing 0
            , Element.width Element.fill
            , Element.height Element.shrink
            ]
            { onChange = EditComment yearMonth
            , text = comment
            , placeholder = Nothing
            , label = labelHidden "Comment"
            }

    else
        Element.text comment


aggregationColumns : List String -> List (IndexedColumn MonthAggregate msg)
aggregationColumns aggregationNames =
    aggregationNames
        |> List.map
            (\agg ->
                T.styledColumn agg (.columns >> Dict.get agg >> Maybe.withDefault 0 >> formatEuro)
            )
