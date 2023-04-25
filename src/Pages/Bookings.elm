module Pages.Bookings exposing (Model, Msg, page)

import CsvParser exposing (Entry)
import Element exposing (Column, Element, alignRight, el, fill, shrink, spacing, table, text, width)
import Element.Font as Font
import Layout exposing (size)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = Noop


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model
    , Cmd.none
    )



-- VIEW


view : Storage -> Model -> View Msg
view storage _ =
    let
        data =
            CsvParser.toEntries storage.rawData
    in
    { title = "Book"
    , body = [ Layout.layout "Book" <| showData data ]
    }


showData : List Entry -> Element Msg
showData data =
    table [ spacing size.s ]
        { data = data
        , columns =
            [ { header = text "ID"
              , width = shrink
              , view = \e -> el [ Font.size Layout.size.s ] <| text e.id
              }
            , { header = text "Date"
              , width = shrink
              , view = \e -> text e.date
              }
            , { header = text "Amount"
              , width = shrink
              , view = \e -> formatEuro e.amount
              }
            , { header = text "Description"
              , width = shrink
              , view = \e -> text e.description
              }
            ]
        }


formatEuro : Int -> Element Msg
formatEuro cents =
    let
        str =
            String.fromInt cents

        ct =
            String.right 2 str

        eur =
            String.slice 0 -2 str

        negative =
            String.left 1 str == "-"

        color =
            if negative then
                [ Font.color Layout.color.red ]

            else
                []

        formatted =
            eur ++ "." ++ ct ++ " €"
    in
    el [ width fill ] <| el ([ alignRight ] ++ color) (text formatted)
