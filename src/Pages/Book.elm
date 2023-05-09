module Pages.Book exposing (Model, Msg, page)

import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, column, el, fill, height, indexedTable, none, padding, shrink, spacing, table, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (color, formatDate, formatEuro, size)
import Maybe.Extra as Maybe
import Page
import Persistence.Data exposing (Account, Data, RawAccountEntry)
import Persistence.Storage as LocalStorage
import Processing.Csv as Csv
import Processing.Model exposing (Entry)
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
    { deletedItems : List ( String, RawAccountEntry ) }


init : ( Model, Cmd Msg )
init =
    ( { deletedItems = [] }, Cmd.none )



-- UPDATE


type Msg
    = Noop


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update s m model =
    ( model, Cmd.none )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    let
        entries =
            Csv.parseEntries <| Dict.values data.rawEntries
    in
    { title = "Book"
    , body = [ Layout.layout "Book" (content model data.accounts entries) ]
    }


content model accounts data =
    column [] [ showData accounts data ]


showData : Dict Int Account -> List Entry -> Element Msg
showData accounts entries =
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
