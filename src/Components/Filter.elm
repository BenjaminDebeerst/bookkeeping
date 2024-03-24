module Components.Filter exposing (..)

import Components.Icons exposing (wand)
import Components.Input exposing (button, disabledButton)
import Components.Tooltip exposing (tooltip)
import Config exposing (color, size)
import Dropdown
import Effect exposing (Effect)
import Element exposing (Element, alignRight, below, column, el, fill, padding, paddingEach, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelLeft, labelRight, placeholder)
import Maybe.Extra
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Processing.BookEntry exposing (Categorization(..))
import Processing.Filter as Processing exposing (any, filterAccount, filterCategory, filterDescription, filterDescriptionRegex, filterMonth, filterYear)
import Processing.Model exposing (getCategoryByShort)
import Regex


type alias Model msg =
    { year : String
    , month : String
    , descr : String
    , descrIsRegex : Bool
    , creatingPattern : Maybe ( Maybe Category, Dropdown.State Category )
    , category : String
    , accounts : List Account
    , categories : List Category
    , onlyUncategorized : Bool
    , lift : Msg -> msg
    }


init : List Account -> List Category -> (Msg -> msg) -> Model msg
init accounts categories lift =
    { year = ""
    , month = ""
    , descr = ""
    , descrIsRegex = False
    , creatingPattern = Nothing
    , category = ""
    , accounts = accounts
    , categories = categories
    , onlyUncategorized = False
    , lift = lift
    }


type Msg
    = Year String
    | Month String
    | Descr String
    | DescrRegex Bool
    | PatternCreateStart
    | PatternCategorySelected (Maybe Category)
    | PatternCategoryDropdown (Dropdown.Msg Category)
    | PatternAbort
    | Category String
    | OnlyUncategorized Bool
    | AddAccount Account
    | RemoveAccount Account
    | SetAccounts (List Account)


update : Msg -> Model msg -> ( Model msg, Effect msg )
update msg model =
    case msg of
        Year year ->
            ( { model | year = year }, Effect.none )

        Month month ->
            ( { model | month = month }, Effect.none )

        Descr descr ->
            ( { model | descr = descr }, Effect.none )

        DescrRegex on ->
            ( { model | descrIsRegex = on, creatingPattern = Nothing }, Effect.none )

        PatternCreateStart ->
            ( { model | creatingPattern = Just ( Nothing, Dropdown.init "" ) }, Effect.none )

        PatternCategoryDropdown dropDownMsg ->
            case model.creatingPattern of
                Nothing ->
                    ( model, Effect.none )

                Just ( selectedCat, dropdownState ) ->
                    let
                        ( state, cmd ) =
                            Dropdown.update (dropdownConfig model.lift model.categories) dropDownMsg model dropdownState
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

        Category cat ->
            ( { model | category = cat }, Effect.none )

        AddAccount acc ->
            ( { model | accounts = acc :: model.accounts }, Effect.none )

        RemoveAccount acc ->
            ( { model | accounts = List.filter (\a -> not <| a.id == acc.id) model.accounts }, Effect.none )

        SetAccounts list ->
            ( { model | accounts = list }, Effect.none )

        OnlyUncategorized b ->
            ( { model | onlyUncategorized = b }, Effect.none )


toFilter : Model msg -> List Processing.Filter
toFilter model =
    []
        ++ (model.year |> String.toInt |> Maybe.map filterYear |> Maybe.Extra.toList)
        ++ (model.month |> String.toInt |> Maybe.map filterMonth |> Maybe.Extra.toList)
        ++ [ if model.descrIsRegex then
                filterDescriptionRegex model.descr

             else
                filterDescription (String.trim model.descr)
           ]
        ++ [ model.accounts |> List.map filterAccount |> any ]
        ++ (getCategoryByShort model.categories model.category |> Maybe.map (\c -> [ filterCategory c ]) |> Maybe.withDefault [])
        ++ [ \bookEntry -> not model.onlyUncategorized || bookEntry.categorization == None ]


yearFilter : Model msg -> Element msg
yearFilter model =
    Input.text [ spacing size.m ]
        { onChange = model.lift << Year
        , text = model.year
        , placeholder = Just <| placeholder [] <| text "Year"
        , label = labelLeft [ padding 0 ] <| text "Year"
        }


monthFilter : Model msg -> Element msg
monthFilter model =
    Input.text [ spacing size.m ]
        { onChange = model.lift << Month
        , text = model.month
        , placeholder = Just <| placeholder [] <| text "Month"
        , label = labelLeft [ padding 0 ] <| text "Month"
        }


descriptionFilter : (Category -> msg) -> (String -> Category -> msg) -> Model msg -> Element msg
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
                { onChange = model.lift << Descr
                , text = model.descr
                , placeholder = Just <| placeholder [] <| text "Description"
                , label = labelLeft [ padding 0 ] <| text "Description"
                }
            , Input.checkbox [ width shrink ]
                { onChange = model.lift << DescrRegex
                , icon = Input.defaultCheckbox
                , checked = model.descrIsRegex
                , label = labelRight [] <| text <| "Regex"
                }
            , if model.descrIsRegex then
                wand [ tooltip below "Create matching pattern from filter", onClick (model.lift PatternCreateStart) ] size.l

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
                            , Dropdown.view (dropdownConfig model.lift model.categories) model dropdown
                            , button (apply selectedCat) "Apply"
                            , button (model.lift PatternAbort) "Abort"
                            , button (save model.descr selectedCat) "Save Pattern"
                            ]
                        ]

                    ( Just ( _, dropdown ), _, _ ) ->
                        [ row [ spacing size.s, width fill, alignRight ]
                            [ el [ alignRight ] <| text "Categorize as: "
                            , Dropdown.view (dropdownConfig model.lift model.categories) model dropdown
                            , disabledButton "Apply"
                            , button (model.lift PatternAbort) "Abort"
                            , disabledButton "Save Pattern"
                            ]
                        ]
               )
        )


categoryFilter : Model msg -> Element msg
categoryFilter model =
    Input.text [ spacing size.m ]
        { onChange = model.lift << Category
        , text = model.category
        , placeholder = Just <| placeholder [] <| text "Category"
        , label = labelLeft [ padding 0 ] <| text "Category"
        }


accountFilter : List Account -> Model msg -> Element msg
accountFilter accounts model =
    Element.row [ spacing size.m, paddingEach { top = size.xs, bottom = 0, left = 0, right = 0 } ]
        ([ text "Accounts"
         , Input.checkbox []
            { onChange =
                \on ->
                    if on then
                        model.lift (SetAccounts accounts)

                    else
                        model.lift (SetAccounts [])
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


accountCheckbox : Model msg -> Account -> Element msg
accountCheckbox model acc =
    Input.checkbox []
        { onChange =
            \add ->
                if add then
                    model.lift <| AddAccount acc

                else
                    model.lift <| RemoveAccount acc
        , icon = Input.defaultCheckbox
        , checked = List.member acc model.accounts
        , label = labelRight [] <| text <| acc.name
        }


uncategorizedFilter : Model msg -> Element msg
uncategorizedFilter model =
    Input.checkbox [ spacing size.m, paddingEach { top = size.xs, bottom = 0, left = 0, right = 0 } ]
        { onChange = model.lift << OnlyUncategorized
        , icon = Input.defaultCheckbox
        , checked = model.onlyUncategorized
        , label = labelLeft [] <| text "Show uncategorized only"
        }


dropdownConfig : (Msg -> msg) -> List Category -> Dropdown.Config Category msg (Model msg)
dropdownConfig msg categories =
    let
        itemToPrompt item =
            text item.name

        itemToElement selected highlighted item =
            el
                [ padding size.s
                , width fill
                , Background.color
                    -- "highlighted" does not work with mouse hover, which creates weird UX, hence ignoring here
                    (if selected then
                        color.extraBrightAccent

                     else
                        color.white
                    )
                ]
                (text item.name)
    in
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
    <|
        Dropdown.withContainerAttributes
            [ Border.color color.grey
            , Border.width 1
            , Border.rounded size.xxs
            , padding size.xs
            ]
        <|
            Dropdown.basic
                { itemsFromModel = always categories
                , selectionFromModel = \m -> m.creatingPattern |> Maybe.andThen Tuple.first
                , dropdownMsg = msg << PatternCategoryDropdown
                , onSelectMsg = msg << PatternCategorySelected
                , itemToPrompt = itemToPrompt
                , itemToElement = itemToElement
                }
