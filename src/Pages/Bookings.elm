module Pages.Bookings exposing (Model, Msg, page)

import Dict
import Element exposing (Column, Element, column, el, fill, none, shrink, spacing, table, text)
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (formatDate, formatEuro, size)
import Maybe.Extra as Maybe
import Page
import Persistence.Data exposing (Data, RawAccountEntry)
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
    = Delete String
    | Undo


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update s m model =
    case m of
        Delete id ->
            let
                deleted =
                    Dict.get id s.rawEntries

                pair =
                    Maybe.map (\v -> ( id, v )) deleted

                toAdd =
                    Maybe.toList pair
            in
            ( { model | deletedItems = toAdd ++ model.deletedItems }
            , LocalStorage.removeEntry s id
            )

        Undo ->
            let
                item =
                    List.map Tuple.second <| List.take 1 model.deletedItems

                cmd =
                    LocalStorage.addEntries s item
            in
            ( { model | deletedItems = List.drop 1 model.deletedItems }
            , cmd
            )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    let
        entries =
            Csv.parseEntries <| Dict.values data.rawEntries
    in
    { title = "Book"
    , body = [ Layout.layout "Book" (content model entries) ]
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
    column [] (undobox ++ [ showData data ])


showData : List Entry -> Element Msg
showData data =
    table [ spacing size.s ]
        { data = data
        , columns =
            [ { header = none
              , width = fill
              , view = \e -> button Layout.style.button { onPress = Just (Delete e.id), label = text "X" }
              }
            , { header = text "ID"
              , width = shrink
              , view = \e -> el [ Font.size Layout.size.s ] <| text <| String.slice 0 8 e.id
              }
            , { header = text "Date"
              , width = shrink
              , view = \e -> text <| formatDate e.date
              }
            , { header = text "Amount"
              , width = shrink
              , view = \e -> formatEuro [] e.amount
              }
            , { header = text "Description"
              , width = shrink
              , view = \e -> text e.description
              }
            ]
        }
