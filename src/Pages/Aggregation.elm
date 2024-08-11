module Pages.Aggregation exposing (Model, Msg, page)

import Components.Dropdown as Dropdown
import Components.Filter as Filter exposing (toAggregateFilter)
import Components.Icons as Icons exposing (edit, plusSquare, triangleDown, triangleUp, xSquare)
import Components.Input exposing (button, disabledButton)
import Components.Table as T exposing (withHeaderActions)
import Components.Tabs as Tabs
import Config exposing (color, size)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, alignRight, below, column, el, fill, indexedTable, minimum, padding, paddingXY, pointer, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (focusedOnLoad, labelHidden, placeholder)
import Html.Events
import Json.Decode as Decode
import Layouts
import Maybe.Extra
import Page exposing (Page)
import Persistence.Account exposing (Account, Accounts)
import Persistence.AggregationGroups as AggregationGroups exposing (AggregationGroup, getAll)
import Persistence.Audits as Audits exposing (Audits)
import Persistence.Category as Category exposing (Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Persistence.RawEntry exposing (RawEntry)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate, startDate, startingBalances)
import Processing.Aggregator as Aggregator exposing (Aggregator, fromAggregationGroup)
import Processing.Filter as Filter exposing (EntryFilter)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (Ordering, aggregateMonth, asc, bookEntryDate, desc)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Util.Formats exposing (formatEuro, formatYearMonth)
import Util.Layout exposing (dataInit, dataUpdate, dataView)
import Util.YearMonth exposing (YearMonth)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> dataInit shared (init [] [] []) initFromData
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
    , ordering : Ordering MonthAggregate
    , tab : Tab
    , edit : Maybe ( YearMonth, String )
    , aggregationBuilder : Maybe Builder
    , customAggregators : List Aggregator
    }


type alias Builder =
    { name : String
    , id : Maybe Int
    , dropdown : Dropdown.Model Category Msg
    }


initBuilder : Data -> Maybe AggregationGroup -> Builder
initBuilder data group =
    let
        name =
            Maybe.map .name group |> Maybe.withDefault ""

        categories : List Category
        categories =
            group
                |> Maybe.map .categories
                |> Maybe.Extra.toList
                |> List.concat
                |> List.filterMap (\id -> Dict.get id data.categories)
    in
    { name = name
    , id = Maybe.map .id group
    , dropdown =
        Dropdown.initMulti
            { id = "group-builder"
            , items = data.categories |> Dict.values
            , selected = categories
            , prompt = prompt
            , label = .name
            , lift = CategoryDropdown
            }
    }


prompt : List Category -> String
prompt items =
    if List.isEmpty items then
        "Select at least 1 category"

    else
        String.concat
            [ List.length items |> String.fromInt
            , " categories selected"
            ]


initFromData : Data -> Model
initFromData data =
    init (Dict.values data.accounts) [] (Dict.values data.rawEntries.entries)


init : List Account -> List AggregationGroup -> List RawEntry -> Model
init accounts groups entries =
    { filters = Filter.init accounts [] (List.map .date entries) Filter
    , ordering = desc aggregateMonth
    , tab = Overview
    , edit = Nothing
    , aggregationBuilder = Nothing
    , customAggregators = List.map fromAggregationGroup groups
    }



-- UPDATE


type Msg
    = Filter Filter.Msg
    | OrderBy (Ordering MonthAggregate)
    | TabSelection Tab
    | EditComment YearMonth String
    | SaveComment
    | BuildAggregation
    | EditAggregation Int
    | AggregationName String
    | CategoryDropdown (Dropdown.Msg Category)
    | SaveAggregation String (List Category)
    | DeleteAggregation Int
    | Abort


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case ( msg, model.aggregationBuilder ) of
        ( Filter filterMsg, _ ) ->
            let
                ( filters, effect ) =
                    Filter.update filterMsg model.filters
            in
            ( { model | filters = filters }, effect )

        ( OrderBy ordering, _ ) ->
            ( { model | ordering = ordering }, Effect.none )

        ( TabSelection tab, _ ) ->
            ( { model | tab = tab }, Effect.none )

        ( EditComment ym comment, _ ) ->
            ( { model | edit = Just ( ym, comment ), aggregationBuilder = Nothing }, Effect.none )

        ( SaveComment, _ ) ->
            let
                updateFn =
                    \( ym, comment ) -> Audits.update ym (\a -> { a | comment = comment }) data.audits

                updated =
                    model.edit
                        |> Maybe.map updateFn
                        |> Maybe.withDefault data.audits
            in
            ( { model | edit = Nothing }, { data | audits = updated } |> Effect.store )

        ( BuildAggregation, Nothing ) ->
            ( { model | aggregationBuilder = Just (initBuilder data Nothing) }, Effect.none )

        ( EditAggregation id, _ ) ->
            ( { model | aggregationBuilder = Just (initBuilder data (AggregationGroups.get id data.aggregationGroups)) }, Effect.none )

        ( BuildAggregation, Just _ ) ->
            ( { model | aggregationBuilder = Nothing }, Effect.none )

        ( AggregationName name, Just builder ) ->
            ( { model | aggregationBuilder = Just { builder | name = name } }, Effect.none )

        ( CategoryDropdown submsg, Just builder ) ->
            let
                ( dropdown, effect ) =
                    Dropdown.update submsg builder.dropdown
            in
            ( { model | aggregationBuilder = Just { builder | dropdown = dropdown } }, effect )

        ( SaveAggregation name categories, Just builder ) ->
            let
                updatedAggregationGroups =
                    case builder.id of
                        Nothing ->
                            AggregationGroups.add name categories data.aggregationGroups

                        Just id ->
                            AggregationGroups.update id name categories data.aggregationGroups
            in
            ( { model | aggregationBuilder = Nothing }
            , { data | aggregationGroups = updatedAggregationGroups } |> Effect.store
            )

        ( DeleteAggregation id, _ ) ->
            let
                updatedAggregationGroups =
                    AggregationGroups.remove id data.aggregationGroups
            in
            ( { model | aggregationBuilder = Nothing }
            , { data | aggregationGroups = updatedAggregationGroups } |> Effect.store
            )

        ( Abort, _ ) ->
            ( { model | edit = Nothing, aggregationBuilder = Nothing }, Effect.none )

        _ ->
            ( model, Effect.none )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    column [ spacing size.m, width fill, padding size.m ]
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
        , tabIcons = \_ -> Nothing
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
        (getAll data.aggregationGroups)
        (groupCreationColumn model)


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
        []
        []


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
        []
        []



-- GROUP CREATION


groupCreationColumn : Model -> List (IndexedColumn a Msg)
groupCreationColumn model =
    let
        idle =
            el [] <| plusSquare [ pointer, onClick BuildAggregation ] size.m

        editing builder =
            el [ below (createGroupDialog builder) ] <| xSquare [ pointer, onClick BuildAggregation ] size.m
    in
    [ T.fullStyledColumn
        (case model.aggregationBuilder of
            -- "below" needs to be attr of a parent to the clickable icon, otherwise the whole "below" is clickable
            Nothing ->
                idle

            Just builder ->
                if Maybe.Extra.isJust builder.id then
                    idle

                else
                    editing builder
        )
        (\_ -> Element.none)
    ]


createGroupDialog : Builder -> Element Msg
createGroupDialog builder =
    el [ padding size.xxs ] <|
        column
            [ spacing size.m
            , width (px 300)
            , padding size.s
            , Border.width 2
            , Border.color color.darkAccent
            , Background.color color.white
            , Font.light
            ]
            [ text "Add aggregation column"
            , Input.text []
                { onChange = AggregationName
                , text = builder.name
                , placeholder = Just <| placeholder [] <| text "Title"
                , label = labelHidden "Title"
                }
            , Dropdown.view builder.dropdown
            , row [ spacing size.m, width fill ]
                ([ button Abort "Abort"
                 , el [ width fill ] Element.none
                 ]
                    ++ (case builder.id of
                            Just id ->
                                [ button (DeleteAggregation id) "Remove" ]

                            Nothing ->
                                []
                       )
                    ++ [ aggregationGroup builder
                            |> Maybe.map (\( s, c ) -> button (SaveAggregation s c) "Save")
                            |> Maybe.withDefault (disabledButton "Save")
                       ]
                )
            ]


aggregationGroup : Builder -> Maybe ( String, List Category )
aggregationGroup builder =
    if builder.name == "" || (builder.dropdown.selected |> List.isEmpty) then
        Nothing

    else
        Just ( builder.name, builder.dropdown.selected )



-- AGGREGATION TABLE


showAggregations : Data -> Model -> List Aggregator -> List AggregationGroup -> List (IndexedColumn MonthAggregate Msg) -> Element Msg
showAggregations data model aggregators aggregationGroups extraColumns =
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
            getEntries data entryFilters (asc bookEntryDate)
                |> aggregate start startSums (aggregators ++ List.map fromAggregationGroup aggregationGroups)
                |> toAggregateFilter model.filters
                |> sortWith model.ordering

        columns =
            aggregationColumns (List.map .title aggregators)
                ++ groupColumns model.aggregationBuilder aggregationGroups
                ++ extraColumns
    in
    showAggResults data.audits model.edit aggregatedData columns


sortWith : Ordering MonthAggregate -> Aggregate -> Aggregate
sortWith ordering agg =
    { agg | rows = agg.rows |> List.sortWith ordering }


viewFilters : Model -> List Account -> Element Msg
viewFilters model accounts =
    column [ spacing size.m ]
        [ Filter.accountFilter accounts model.filters
        , Filter.dateRangeFilter model.filters
        ]


showAggResults : Audits -> Maybe ( YearMonth, String ) -> Aggregate -> List (IndexedColumn MonthAggregate Msg) -> Element Msg
showAggResults audits edit aggregation extraColumns =
    el [ width fill, paddingXY 0 size.m ] <|
        indexedTable T.style.fullWidthTable
            { data = aggregation.rows
            , columns =
                (T.textColumn "Month" (.month >> formatYearMonth)
                    |> withHeaderActions
                        [ triangleUp [ pointer, onClick (OrderBy (asc aggregateMonth)) ] size.s
                        , triangleDown [ pointer, onClick (OrderBy (desc aggregateMonth)) ] size.s
                        ]
                )
                    :: extraColumns
                    ++ [ T.styledColumn "Comment" (.month >> commentCell audits edit) |> T.withColumnWidth fill ]
            }


commentCell : Audits -> Maybe ( YearMonth, String ) -> YearMonth -> Element Msg
commentCell audits edit yearMonth =
    let
        comment =
            Audits.get yearMonth audits |> .comment

        default =
            row [ spacing size.m, width (fill |> minimum 500), onClick (EditComment yearMonth comment), pointer ]
                [ Element.paragraph [] [ text comment ]
                , Icons.edit [ alignRight, onClick (EditComment yearMonth comment), pointer ] size.m
                ]
    in
    case edit of
        Nothing ->
            default

        Just ( ym, edited ) ->
            if ym /= yearMonth then
                default

            else
                row [ spacing size.m, width fill ]
                    [ Input.multiline
                        [ Element.padding 1
                        , Border.rounded 3
                        , Background.color (Element.rgb 1 1 1)
                        , Border.width 0
                        , Element.spacing 0
                        , Element.width Element.fill
                        , Element.height Element.shrink
                        , focusedOnLoad
                        , onKey [ ( "Enter", SaveComment ), ( "Escape", Abort ) ]
                        ]
                        { onChange = EditComment yearMonth
                        , text = edited
                        , placeholder = Nothing
                        , label = labelHidden "Comment"
                        , spellcheck = False
                        }
                    , Icons.checkMark [ alignRight, onClick SaveComment, pointer ] size.m
                    , Icons.cross [ alignRight, onClick Abort, pointer ] size.m
                    ]


onKey : List ( String, msg ) -> Element.Attribute msg
onKey l =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        l
                            |> List.filterMap
                                (\( k, m ) ->
                                    if k == key then
                                        Just (Decode.succeed m)

                                    else
                                        Nothing
                                )
                            |> List.head
                            |> Maybe.withDefault (Decode.fail "Wrong key")
                    )
            )
        )


aggregationColumns : List String -> List (IndexedColumn MonthAggregate msg)
aggregationColumns aggregationNames =
    aggregationNames
        |> List.map
            (\agg ->
                T.styledColumn agg (.columns >> Dict.get agg >> Maybe.withDefault 0 >> formatEuro)
            )


groupColumns : Maybe Builder -> List AggregationGroup -> List (IndexedColumn MonthAggregate Msg)
groupColumns maybeBuilder groups =
    let
        idle group =
            row [ spacing size.m, width fill ]
                [ text group.name
                , el [ alignRight ] <| edit [ pointer, onClick (EditAggregation group.id) ] size.m
                ]

        editing group builder =
            row [ spacing size.m, width fill ]
                [ text group.name
                , el [ alignRight, below (createGroupDialog builder) ] <| edit [] size.m
                ]
    in
    groups
        |> List.map
            (\group ->
                T.fullStyledColumn
                    (case maybeBuilder of
                        -- "below" needs to be attr of a parent to the clickable icon, otherwise the whole "below" is clickable
                        Nothing ->
                            idle group

                        Just builder ->
                            case builder.id of
                                Just theId ->
                                    if theId == group.id then
                                        editing group builder

                                    else
                                        idle group

                                Nothing ->
                                    idle group
                    )
                    (.columns >> Dict.get group.name >> Maybe.withDefault 0 >> formatEuro)
            )
