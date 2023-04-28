module Pages.Bookings exposing (Model, Msg, page, showData)

import CsvParser exposing (Entry)
import Dict
import Element exposing (Column, Element, alignRight, column, el, fill, none, shrink, spacing, table, text, width)
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (size)
import Maybe.Extra as Maybe
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
    { deletedItems : List ( String, String ) }


init : ( Model, Cmd Msg )
init =
    ( { deletedItems = [] }, Cmd.none )



-- UPDATE


type Msg
    = Delete String
    | Undo


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update s m model =
    case m of
        Delete id ->
            let
                deleted =
                    Dict.get id s.rawData

                pair =
                    Maybe.map (\v -> ( id, v )) deleted

                toAdd =
                    Maybe.toList pair
            in
            ( { model | deletedItems = toAdd ++ model.deletedItems }
            , Storage.remove s id
            )

        Undo ->
            let
                item =
                    List.map Tuple.second <| List.take 1 model.deletedItems

                cmd =
                    Storage.addRows s item
            in
            ( { model | deletedItems = List.drop 1 model.deletedItems }
            , cmd
            )



-- VIEW


view : Storage -> Model -> View Msg
view storage model =
    let
        data =
            CsvParser.toEntries storage.rawData
    in
    { title = "Book"
    , body = [ Layout.layout "Book" (content model data) ]
    }


content model data =
    let
        undobox =
            if List.isEmpty model.deletedItems then
                []

            else
                [ button Layout.style.button
                    { onPress = Just Undo
                    , label = text <| "Undo (" ++ (String.fromInt <| List.length model.deletedItems) ++ ")"
                    }
                ]
    in
    column [] (undobox ++ [ showData (Just Delete) data ])


showData : Maybe (String -> msg) -> List Entry -> Element msg
showData delete data =
    table [ spacing size.s ]
        { data = data
        , columns =
            [ { header = none
              , width = fill
              , view = \e -> button Layout.style.button { onPress = Maybe.map (\f -> f e.id) delete, label = text "X" }
              }
            , { header = text "ID"
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


formatEuro : Int -> Element msg
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