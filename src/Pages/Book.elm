module Pages.Book exposing (Model, Msg, page)

import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, centerX, centerY, column, el, explain, fill, height, indexedTable, padding, paddingXY, shrink, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelLeft, placeholder)
import Layout exposing (color, formatDate, formatEuro, size)
import Maybe.Extra
import Page
import Persistence.Data exposing (Account, Data, RawAccountEntry)
import Processing.Csv exposing (Entry)
import Processing.Model exposing (dateAsc, dateDesc, filterDescription, filterMonth, filterYear, getEntries)
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
    }


init : ( Model, Cmd Msg )
init =
    ( { year = "", month = "", descr = "" }, Cmd.none )



-- UPDATE


type Msg
    = FilterYear String
    | FilterMonth String
    | FilterDescr String


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        FilterYear year ->
            ( { model | year = year }, Cmd.none )

        FilterMonth month ->
            ( { model | month = month }, Cmd.none )

        FilterDescr descr ->
            ( { model | descr = descr }, Cmd.none )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    let
        filters =
            []
                ++ (model.year |> String.toInt |> Maybe.map filterYear |> Maybe.Extra.toList)
                ++ (model.month |> String.toInt |> Maybe.map filterMonth |> Maybe.Extra.toList)
                ++ [ model.descr |> String.trim |> filterDescription ]

        entries =
            getEntries data filters dateDesc
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
showFilters model accounts =
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


showData : Dict Int Account -> List Entry -> Element Msg
showData accounts entries =
    column [ width shrink ]
        [ dataTable accounts entries
        , maybeNoEntries <| List.length entries
        ]


dataTable accounts entries =
    indexedTable [ spacing size.tiny ]
        { data = entries
        , columns =
            [ { header = header "Date"
              , width = shrink
              , view = \i e -> row i <| text <| formatDate e.date
              }
            , { header = header "Amount"
              , width = shrink
              , view = \i e -> row i <| formatEuro [] e.amount
              }
            , { header = header "Description"
              , width = shrink
              , view = \i e -> row i <| text e.description
              }
            , { header = header "Account"
              , width = shrink
              , view = \i e -> row i <| (Dict.get e.account accounts |> Maybe.map .name |> Maybe.withDefault "Not Found" |> text)
              }
            ]
        }


maybeNoEntries n =
    if n == 0 then
        el [ centerX, Font.italic, padding size.s ] <| text "No book entries found"

    else
        Element.none


header : String -> Element msg
header s =
    el headerStyle <| text s


row : Int -> Element msg -> Element msg
row i e =
    el (rowStyle i) e


headerStyle : List (Attribute msg)
headerStyle =
    [ Background.color color.brightAccent
    , Font.bold
    , Font.color color.black
    , padding size.xs
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
