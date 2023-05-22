module Pages.Book exposing (Model, Msg, page)

import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Column, Element, alignLeft, alignRight, centerX, column, el, fill, height, indexedTable, padding, paddingXY, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelLeft, labelRight, placeholder)
import Html exposing (summary)
import Icons exposing (triangleDown, triangleUp)
import Layout exposing (color, formatDate, formatEuro, formatEuroStr, size, style)
import Maybe.Extra
import Page
import Persistence.Data exposing (Account, Category, Data, RawEntry)
import Persistence.Storage as Storage exposing (addEntries)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)
import Processing.CategoryParser as Parser
import Processing.Filter exposing (Filter, filterCategory, filterDescription, filterMonth, filterYear)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (Ordering, asc, dateAsc, dateDesc, desc)
import Request exposing (Request)
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { year : String
    , month : String
    , descr : String
    , categoryFilter : String
    , ordering : Ordering BookEntry
    , editCategories : Bool
    , categoryEdits : Dict String CatAttempt
    , onlyUncategorized : Bool
    }


type CatAttempt
    = Unknown String
    | Known String Categorization


init : ( Model, Cmd Msg )
init =
    ( { year = ""
      , month = ""
      , descr = ""
      , categoryFilter = ""
      , ordering = dateAsc
      , editCategories = False
      , categoryEdits = Dict.empty
      , onlyUncategorized = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FilterYear String
    | FilterMonth String
    | FilterDescr String
    | FilterCategory String
    | OnlyUncategorized Bool
    | OrderBy (Ordering BookEntry)
    | Categorize
    | EditCategory String String
    | SaveCategories
    | AbortCategorize


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        FilterYear year ->
            ( { model | year = year }, Cmd.none )

        FilterMonth month ->
            ( { model | month = month }, Cmd.none )

        FilterDescr descr ->
            ( { model | descr = descr }, Cmd.none )

        FilterCategory cat ->
            ( { model | categoryFilter = cat }, Cmd.none )

        OnlyUncategorized b ->
            ( { model | onlyUncategorized = b }, Cmd.none )

        OrderBy ordering ->
            ( { model | ordering = ordering }, Cmd.none )

        Categorize ->
            ( { model | editCategories = True }, Cmd.none )

        AbortCategorize ->
            ( { model | editCategories = False }, Cmd.none )

        EditCategory id cat ->
            ( { model | categoryEdits = Dict.insert id (parseCategorization data cat) model.categoryEdits }, Cmd.none )

        SaveCategories ->
            let
                entryCategorizations : Dict String Categorization
                entryCategorizations =
                    Dict.Extra.filterMap
                        (\_ ca ->
                            case ca of
                                Unknown "" ->
                                    Just None

                                Unknown _ ->
                                    Nothing

                                Known _ cat ->
                                    Just cat
                        )
                        model.categoryEdits

                alteredCategories =
                    Dict.Extra.filterMap (\k v -> Dict.get k data.rawEntries |> Maybe.map (\e -> { e | categorization = toPersistence v })) entryCategorizations

                editedEntries : List RawEntry
                editedEntries =
                    alteredCategories
                        |> Dict.values
            in
            ( { model | editCategories = False, categoryEdits = Dict.empty }
            , addEntries editedEntries data |> Storage.store
            )


view : Data -> Model -> View Msg
view data model =
    let
        filters =
            []
                ++ (model.year |> String.toInt |> Maybe.map filterYear |> Maybe.Extra.toList)
                ++ (model.month |> String.toInt |> Maybe.map filterMonth |> Maybe.Extra.toList)
                ++ [ model.descr |> String.trim |> filterDescription ]
                ++ (getCategoryByShort data model.categoryFilter |> Maybe.map (\c -> [ filterCategory c ]) |> Maybe.withDefault [])
                ++ [ \bookEntry -> not model.onlyUncategorized || bookEntry.categorization == None ]

        entries =
            getEntries data filters model.ordering
    in
    { title = "Book"
    , body = [ Layout.layout "Book" (content model data entries) ]
    }


content : Model -> Data -> List BookEntry -> Element Msg
content model data entries =
    column [ spacing size.m ]
        [ showFilters model data.accounts
        , showActions model
        , showData model entries
        ]


showFilters : Model -> Dict Int Account -> Element Msg
showFilters model _ =
    column [ spacing size.s ]
        [ el [ Font.bold, Font.size size.m ] <| text "Filters"
        , Input.text []
            { onChange = FilterYear
            , text = model.year
            , placeholder = Just <| placeholder [] <| text "Year"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Year"
            }
        , Input.text []
            { onChange = FilterMonth
            , text = model.month
            , placeholder = Just <| placeholder [] <| text "Month"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Month"
            }
        , Input.text []
            { onChange = FilterDescr
            , text = model.descr
            , placeholder = Just <| placeholder [] <| text "Description"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Description"
            }
        , Input.text []
            { onChange = FilterCategory
            , text = model.categoryFilter
            , placeholder = Just <| placeholder [] <| text "Category"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Category"
            }
        , Input.checkbox []
            { onChange = OnlyUncategorized
            , icon = Input.defaultCheckbox
            , checked = model.onlyUncategorized
            , label = labelLeft [ paddingXY size.m size.xs ] <| text "Show uncategorized only"
            }
        ]


showActions : Model -> Element Msg
showActions model =
    Element.row [ spacing size.s ]
        ([]
            ++ [ Input.button style.button { onPress = Just Categorize, label = text "Edit Categories" } ]
            ++ (if model.editCategories then
                    [ Input.button style.button { onPress = Just SaveCategories, label = text "Save Category Edits" }
                    , Input.button style.button { onPress = Just AbortCategorize, label = text "Abort" }
                    ]

                else
                    []
               )
        )


showData : Model -> List BookEntry -> Element Msg
showData model entries =
    column [ width shrink, spacing size.m ]
        [ summary entries
        , dataTable model entries
        , maybeNoEntries <| List.length entries
        ]


summary entries =
    let
        n =
            List.length entries

        sum =
            List.foldl (\e s -> s + e.amount) 0 entries

        avg =
            sum // n
    in
    el [] <|
        text <|
            String.join " "
                [ "Showing"
                , n |> String.fromInt
                , "entries. Sum:"
                , sum |> formatEuroStr
                , ". Average amount:"
                , formatEuroStr avg
                , "."
                ]


dataTable model entries =
    indexedTable [ spacing size.tiny ]
        { data = entries
        , columns =
            [ { header = header (OrderBy dateAsc) (OrderBy dateDesc) "Date"
              , width = shrink
              , view = \i e -> row i <| text <| formatDate e.date
              }
            , { header = header (OrderBy (asc .amount)) (OrderBy (desc .amount)) "Amount"
              , width = shrink
              , view = \i e -> row i <| formatEuro [] e.amount
              }
            , { header = header (OrderBy (asc <| .categorization >> categorizationString Full)) (OrderBy (desc <| .categorization >> categorizationString Full)) "Category"
              , width = shrink
              , view = \i e -> row i <| categoryCell model e
              }
            , { header = header (OrderBy (asc .description)) (OrderBy (desc .description)) "Description"
              , width = shrink
              , view = \i e -> row i <| text e.description
              }
            , { header = header (OrderBy (asc accountName)) (OrderBy (desc accountName)) "Account"
              , width = shrink
              , view = \i e -> row i <| text e.account.name
              }
            ]
        }


categoryCell : Model -> BookEntry -> Element Msg
categoryCell model entry =
    if model.editCategories then
        Input.text []
            { onChange = EditCategory entry.id
            , text = categoryInputText model.categoryEdits entry
            , placeholder = Nothing
            , label = labelHidden "Categorization"
            }

    else
        text <| categorizationString Full entry.categorization


categoryInputText : Dict String CatAttempt -> BookEntry -> String
categoryInputText categoryEdits entry =
    case Dict.get entry.id categoryEdits of
        Just (Unknown edit) ->
            edit

        Just (Known edit _) ->
            edit

        Nothing ->
            categorizationString Short entry.categorization


type CategoryPresentation
    = Full
    | Short


categorizationString : CategoryPresentation -> Categorization -> String
categorizationString p c =
    case ( c, p ) of
        ( None, _ ) ->
            ""

        ( Single cat, Short ) ->
            cat.short

        ( Single cat, Full ) ->
            cat.name

        ( Split cats, Short ) ->
            cats
                |> List.map (\s -> s.category.short ++ " " ++ String.fromInt s.amount)
                |> List.sort
                |> String.join " "

        ( Split cats, Full ) ->
            cats
                |> List.map (\es -> es.category.name ++ " " ++ String.fromInt es.amount)
                |> List.sort
                |> String.join "\n"


accountName : BookEntry -> String
accountName e =
    e.account.name


maybeNoEntries n =
    if n == 0 then
        el [ centerX, Font.italic, padding size.s ] <| text "No book entries found"

    else
        Element.none


header : msg -> msg -> String -> Element msg
header up down s =
    Element.row headerStyle
        [ el [ alignLeft ] <| text s
        , column
            [ alignRight, height fill, spacing size.xxs ]
            [ triangleUp [ onClick up ] size.s
            , triangleDown [ onClick down ] size.s
            ]
        ]


row : Int -> Element msg -> Element msg
row i e =
    el (rowStyle i) e


headerStyle : List (Attribute msg)
headerStyle =
    [ Background.color color.brightAccent
    , Font.bold
    , Font.color color.black
    , Font.size size.m
    , padding size.xs
    , spacing size.xs
    ]


rowStyle : Int -> List (Attribute msg)
rowStyle i =
    let
        bgColor =
            if modBy 2 i == 1 then
                color.white

            else
                color.extraBrightAccent
    in
    [ Background.color bgColor
    , height fill
    , padding size.xs
    ]


parseCategorization : Data -> String -> CatAttempt
parseCategorization data string =
    case Parser.parseCategorization string of
        Ok Parser.Empty ->
            Known string None

        Ok (Parser.One shortName) ->
            getCategoryByShort data shortName
                |> Maybe.map (Known string << Single)
                |> Maybe.withDefault (Unknown string)

        Ok (Parser.Multiple list) ->
            case
                list
                    |> List.map (categoryForTuple data)
                    |> Maybe.Extra.combine
            of
                Just categories ->
                    Known string (Split categories)

                Nothing ->
                    Unknown string

        Err _ ->
            Unknown string


categoryForTuple : Data -> ( String, Int ) -> Maybe EntrySplit
categoryForTuple data ( string, int ) =
    getCategoryByShort data string |> Maybe.map (\c -> EntrySplit c int)


getCategoryByShort : Data -> String -> Maybe Category
getCategoryByShort data string =
    Dict.values data.categories
        |> List.filter (\c -> c.short == string)
        |> List.head
