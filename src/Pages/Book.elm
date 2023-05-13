module Pages.Book exposing (Model, Msg, page)

import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Column, Element, alignLeft, alignRight, centerX, column, el, fill, height, indexedTable, padding, paddingXY, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelLeft, placeholder)
import Icons exposing (triangleDown, triangleUp)
import Layout exposing (color, formatDate, formatEuro, size, style)
import Maybe.Extra
import Page
import Persistence.Data exposing (Account, Category, Data, RawEntry)
import Persistence.Storage as Storage exposing (addEntries)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)
import Processing.Filter exposing (filterDescription, filterMonth, filterYear)
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
    , ordering : Ordering BookEntry
    , editCategories : Bool
    , categoryEdits : Dict String CatAttempt
    , debug : List String
    }


type CatAttempt
    = Unknown String
    | Known Categorization


init : ( Model, Cmd Msg )
init =
    ( { year = ""
      , month = ""
      , descr = ""
      , ordering = dateAsc
      , editCategories = False
      , categoryEdits = Dict.empty
      , debug = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FilterYear String
    | FilterMonth String
    | FilterDescr String
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

                                Known cat ->
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
        , showDebug data model
        , showActions model
        , showData model entries
        ]


showDebug : Data -> Model -> Element msg
showDebug data model =
    column [ spacing size.m ]
        (List.map text model.debug
            ++ [ text <| Debug.toString model.categoryEdits ]
        )


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
    column [ width shrink ]
        [ dataTable model entries
        , maybeNoEntries <| List.length entries
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
            , { header = header (OrderBy (asc accountName)) (OrderBy (desc accountName)) "Account" -- TODO actually sort by account
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
            , text = categoryCellText model.categoryEdits entry
            , placeholder = Nothing
            , label = labelHidden "Categorization"
            }

    else
        text <|
            case entry.categorization of
                None ->
                    ""

                Single category ->
                    category.name

                Split _ ->
                    "split"


categoryCellText : Dict String CatAttempt -> BookEntry -> String
categoryCellText categoryEdits entry =
    case Dict.get entry.id categoryEdits of
        Just (Unknown edit) ->
            edit

        Just (Known (Single cat)) ->
            cat.short

        Just (Known (Split l)) ->
            l |> List.map (\i -> i.category.short ++ " " ++ String.fromInt i.amount) |> String.join ", "

        Just (Known None) ->
            ""

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
            cats |> List.map (.category >> .short) |> List.sort |> String.join ", "

        ( Split cats, Full ) ->
            cats |> List.map (.category >> .name) |> List.sort |> String.join ", "


toString : EntrySplit -> String
toString s =
    s.category.short ++ " " ++ String.fromInt s.amount


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


{-| No support for split categorization yet
-}
parseCategorization : Data -> String -> CatAttempt
parseCategorization data string =
    Dict.values data.categories
        |> List.filter (\c -> c.short == string)
        |> List.head
        |> Maybe.map (\c -> Known (Single c))
        |> Maybe.withDefault
            (if string == "" then
                Known None

             else
                Unknown string
            )
