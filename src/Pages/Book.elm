module Pages.Book exposing (Model, Msg, page)

import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, alignLeft, alignRight, centerX, column, el, fill, height, indexedTable, padding, paddingXY, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelLeft, placeholder)
import Icons exposing (triangleDown, triangleUp)
import Layout exposing (color, formatDate, formatEuro, size)
import Maybe.Extra
import Page
import Persistence.Data exposing (Account, Data)
import Processing.BookEntry exposing (BookEntry)
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
    }


init : ( Model, Cmd Msg )
init =
    ( { year = ""
      , month = ""
      , descr = ""
      , ordering = dateAsc
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FilterYear String
    | FilterMonth String
    | FilterDescr String
    | OrderBy (Ordering BookEntry)


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        FilterYear year ->
            ( { model | year = year }, Cmd.none )

        FilterMonth month ->
            ( { model | month = month }, Cmd.none )

        FilterDescr descr ->
            ( { model | descr = descr }, Cmd.none )

        OrderBy ordering ->
            ( { model | ordering = ordering }, Cmd.none )


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
    , body = [ Layout.layout "Book" (content model data.accounts entries) ]
    }


content model accounts data =
    column [ spacing size.m ]
        [ showFilters model accounts
        , showData accounts data
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
        ]


showData : Dict Int Account -> List BookEntry -> Element Msg
showData accounts entries =
    column [ width shrink ]
        [ dataTable accounts entries
        , maybeNoEntries <| List.length entries
        ]


dataTable accounts entries =
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
