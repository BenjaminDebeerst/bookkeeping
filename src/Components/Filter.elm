module Components.Filter exposing (Model, Msg(..), accountFilter, categoryFilter, dateRangeFilter, descriptionFilter, getDateRange, init, toAggregateDateFilter, toEntryFilter, update)

import Components.Icons exposing (wand)
import Components.Input exposing (button, disabledButton)
import Components.RangeSlider as RangeSlider exposing (Selection(..))
import Components.Tooltip exposing (tooltip)
import Config exposing (color, size)
import Dropdown exposing (withPromptElement, withSearchAttributes)
import Effect exposing (Effect)
import Element exposing (Element, alignRight, below, column, el, fill, minimum, padding, paddingEach, rgb, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelLeft, labelRight, placeholder)
import List.Extra
import Maybe.Extra
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category, CategoryGroup(..), category)
import Processing.BookEntry exposing (Categorization(..))
import Processing.Filter as Filter exposing (AggregateFilter, any, filterAccount, filterAggregateMonthRange, filterCategory, filterDescription, filterDescriptionRegex, filterEntryMonthRange)
import Regex
import Time.Date exposing (Date)
import Util.Formats exposing (formatYearMonthNumeric)
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias Model =
    { dateRange : RangeSlider.Model YearMonth
    , descr : String
    , descrIsRegex : Bool
    , creatingPattern : Maybe ( Maybe Category, Dropdown.State Category )
    , category : Category
    , categoryDropdown : Dropdown.State Category
    , accounts : List Account
    , allCategories : List Category
    , filterCategories : List Category
    }


init : List Account -> List Category -> List Date -> Maybe ( Date, Date ) -> Model
init accounts categories dates initialRange =
    let
        ( dateRangeStart, tail ) =
            dateRange dates accounts |> List.Extra.uncons |> Maybe.withDefault ( YearMonth.zero, [] )

        initialSelection : Selection YearMonth
        initialSelection =
            initialRange |> Maybe.map (\( min, max ) -> Range (YearMonth.fromDate min) (YearMonth.fromDate max)) |> Maybe.withDefault (Last 12)
    in
    { dateRange = RangeSlider.init dateRangeStart tail True initialSelection
    , descr = ""
    , descrIsRegex = False
    , creatingPattern = Nothing
    , category = allCategories
    , categoryDropdown = Dropdown.init "category-filter"
    , accounts = accounts
    , allCategories = categories
    , filterCategories = categories
    }


dateRange : List Date -> List Account -> List YearMonth
dateRange dates accounts =
    YearMonth.range
        ((dates |> List.map YearMonth.fromDate)
            ++ List.map (.start >> .yearMonth) accounts
        )


type Msg
    = DateRange (RangeSlider.Selection YearMonth)
    | Descr String
    | DescrRegex Bool
    | PatternCreateStart
    | PatternCategorySelected (Maybe Category)
    | PatternCategoryDropdown (Dropdown.Msg Category)
    | PatternAbort
    | CategorySelected Category
    | CategoryDropDown (Dropdown.Msg Category)
    | AddAccount Account
    | RemoveAccount Account
    | SetAccounts (List Account)
    | ApplyPattern Category
    | SavePattern String Category


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        DateRange subMsg ->
            ( { model | dateRange = RangeSlider.update subMsg model.dateRange }, Effect.none )

        Descr descr ->
            ( { model | descr = descr }, Effect.none )

        DescrRegex on ->
            ( { model | descrIsRegex = on, creatingPattern = Nothing }, Effect.none )

        PatternCreateStart ->
            ( { model | creatingPattern = Just ( Nothing, Dropdown.init "category-for-pattern" ) }, Effect.none )

        PatternCategoryDropdown dropDownMsg ->
            case model.creatingPattern of
                Nothing ->
                    ( model, Effect.none )

                Just ( selectedCat, dropdownState ) ->
                    let
                        ( state, cmd ) =
                            Dropdown.update (patternCategoryDropdownConfig model) dropDownMsg model dropdownState
                    in
                    ( { model | creatingPattern = Just ( selectedCat, state ) }, Effect.sendCmd cmd )

        PatternCategorySelected cat ->
            case model.creatingPattern of
                Nothing ->
                    ( model, Effect.none )

                Just ( _, state ) ->
                    ( { model | creatingPattern = Just ( cat, state ) }, Effect.none )

        PatternAbort ->
            ( { model | creatingPattern = Nothing }, Effect.none )

        CategorySelected cat ->
            ( { model | category = cat }, Effect.none )

        CategoryDropDown dropdownMsg ->
            let
                ( updated, cmd ) =
                    Dropdown.update (categoryFilterDropdownConfig model) dropdownMsg model model.categoryDropdown
            in
            ( { model | categoryDropdown = updated }, Effect.sendCmd cmd )

        AddAccount acc ->
            ( { model | accounts = acc :: model.accounts }, Effect.none )

        RemoveAccount acc ->
            ( { model | accounts = List.filter (\a -> not <| a.id == acc.id) model.accounts }, Effect.none )

        SetAccounts list ->
            ( { model | accounts = list }, Effect.none )

        ApplyPattern _ ->
            ( model, Effect.none )

        SavePattern _ _ ->
            ( model, Effect.none )


toEntryFilter : Model -> List Filter.EntryFilter
toEntryFilter model =
    []
        ++ [ filterEntryMonthRange (RangeSlider.min model.dateRange) (RangeSlider.max model.dateRange) ]
        ++ [ if model.descrIsRegex then
                filterDescriptionRegex model.descr

             else
                filterDescription (String.trim model.descr)
           ]
        ++ [ model.accounts |> List.map filterAccount |> any ]
        ++ (if model.category == allCategories then
                []

            else if model.category == uncategorized then
                [ \bookEntry -> bookEntry.categorization == None ]

            else
                [ filterCategory model.category ]
           )


toAggregateDateFilter : Model -> Filter.AggregateFilter
toAggregateDateFilter model =
    filterAggregateMonthRange (RangeSlider.min model.dateRange) (RangeSlider.max model.dateRange)


dateRangeFilter : Model -> Element Msg
dateRangeFilter model =
    RangeSlider.view "Date range" formatYearMonthNumeric model.dateRange |> Element.map DateRange


descriptionFilter : (Category -> Msg) -> (String -> Category -> Msg) -> Model -> Element Msg
descriptionFilter apply save model =
    column [ width fill, spacing size.s ]
        ([ let
            inputBorder =
                if model.descrIsRegex && (Regex.fromString model.descr |> Maybe.Extra.isNothing) then
                    [ Border.color color.red ]

                else
                    []
           in
           row [ spacing size.m, width fill ]
            [ Input.text ([ spacing size.m ] ++ inputBorder)
                { onChange = Descr
                , text = model.descr
                , placeholder = Just <| placeholder [] <| text "Description"
                , label = labelLeft [ padding 0 ] <| text "Description"
                }
            , Input.checkbox [ width shrink ]
                { onChange = DescrRegex
                , icon = Input.defaultCheckbox
                , checked = model.descrIsRegex
                , label = labelRight [] <| text <| "Regex"
                }
            , if model.descrIsRegex then
                wand [ tooltip below "Create matching pattern from filter", onClick PatternCreateStart ] size.l

              else
                wand [ tooltip below "Use Regex to create matching pattern from filter", Font.color color.grey ] size.l
            ]
         ]
            ++ (case ( model.creatingPattern, String.isEmpty model.descr, Regex.fromString model.descr ) of
                    ( Nothing, _, _ ) ->
                        []

                    ( Just ( Just selectedCat, dropdown ), False, Just validRegex ) ->
                        [ row [ spacing size.s, width fill, alignRight ]
                            [ el [ alignRight ] <| text "Categorize as: "
                            , Dropdown.view (patternCategoryDropdownConfig model) model dropdown
                            , button (apply selectedCat) "Apply"
                            , button PatternAbort "Abort"
                            , button (save model.descr selectedCat) "Save Pattern"
                            ]
                        ]

                    ( Just ( _, dropdown ), _, _ ) ->
                        [ row [ spacing size.s, width fill, alignRight ]
                            [ el [ alignRight ] <| text "Categorize as: "
                            , Dropdown.view (patternCategoryDropdownConfig model) model dropdown
                            , disabledButton "Apply"
                            , button PatternAbort "Abort"
                            , disabledButton "Save Pattern"
                            ]
                        ]
               )
        )


categoryFilter : Model -> Element Msg
categoryFilter model =
    row [ spacing size.m ]
        [ text "Category"
        , Dropdown.view (categoryFilterDropdownConfig model) model model.categoryDropdown
        ]


accountFilter : List Account -> Model -> Element Msg
accountFilter accounts model =
    Element.row [ spacing size.m, paddingEach { top = size.xs, bottom = 0, left = 0, right = 0 } ]
        ([ text "Accounts"
         , Input.checkbox []
            { onChange =
                \on ->
                    if on then
                        SetAccounts accounts

                    else
                        SetAccounts []
            , icon = Input.defaultCheckbox
            , checked = List.length model.accounts == List.length accounts
            , label = labelRight [] <| text <| "All"
            }
         ]
            ++ List.map
                (\acc ->
                    accountCheckbox model acc
                )
                accounts
        )


accountCheckbox : Model -> Account -> Element Msg
accountCheckbox model acc =
    Input.checkbox []
        { onChange =
            \add ->
                if add then
                    AddAccount acc

                else
                    RemoveAccount acc
        , icon = Input.defaultCheckbox
        , checked = List.member acc model.accounts
        , label = labelRight [] <| text <| acc.name
        }


patternCategoryDropdownConfig : Model -> Dropdown.Config Category Msg Model
patternCategoryDropdownConfig model =
    Dropdown.filterable
        { itemsFromModel = .allCategories
        , selectionFromModel = .creatingPattern >> Maybe.andThen Tuple.first
        , dropdownMsg = PatternCategoryDropdown
        , onSelectMsg = PatternCategorySelected
        , itemToPrompt = .name >> itemPrompt
        , itemToElement = itemElement
        , itemToText = \c -> c.name ++ "|" ++ c.short
        }
        |> dropdownListStyle
        |> withPromptElement (itemPrompt "-- Select --")
        |> withSearchAttributes [ width (shrink |> minimum 120) ]


categoryFilterDropdownConfig : Model -> Dropdown.Config Category Msg Model
categoryFilterDropdownConfig model =
    Dropdown.filterable
        { itemsFromModel = \m -> allCategories :: uncategorized :: m.filterCategories
        , selectionFromModel = .category >> Just
        , dropdownMsg = CategoryDropDown
        , onSelectMsg = Maybe.withDefault allCategories >> CategorySelected
        , itemToPrompt = .name >> itemPrompt
        , itemToElement = itemElement
        , itemToText = \c -> c.name ++ "|" ++ c.short
        }
        |> dropdownListStyle
        |> withPromptElement (text allCategories.name)
        |> withSearchAttributes [ width (shrink |> minimum 120) ]


itemPrompt : String -> Element msg
itemPrompt s =
    el
        [ Border.width 1
        , Border.rounded 3
        , Border.color color.grey
        , Background.color (rgb 1 1 1)
        , padding size.s
        , width (shrink |> minimum 120)
        ]
        (text s)


itemElement : Bool -> Bool -> Category -> Element msg
itemElement selected highlighted item =
    el
        [ padding size.s
        , width fill
        , Background.color
            (if highlighted then
                color.brightAccent

             else if selected then
                color.extraBrightAccent

             else
                color.white
            )
        ]
        (text item.name)


dropdownListStyle =
    Dropdown.withListAttributes
        [ Background.color color.white
        , Border.color color.grey
        , Border.width 1
        , Border.shadow
            { offset = ( 0, 1 )
            , size = 0.01
            , blur = 5
            , color = color.black
            }
        ]


allCategories =
    category -1 "-- All --" "" Internal []


uncategorized =
    category -2 "-- Uncategorized --" "" Internal []


getDateRange : Model -> ( Date, Date )
getDateRange model =
    RangeSlider.getRange model.dateRange |> Tuple.mapBoth YearMonth.toDate YearMonth.toDate
