module Pages.CsvImport exposing (Model, Msg, page)

import Dict
import Element exposing (Element, column, el, fill, height, maximum, minimum, px, spacing, text, width)
import Element.Input as Input exposing (labelAbove, placeholder)
import Layout
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
    { textinput : String
    }


init : ( Model, Cmd Msg )
init =
    ( { textinput = "" }, Cmd.none )


type Msg
    = Textinput String
    | StoreData
    | DeleteAllData


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        Textinput s ->
            ( { model | textinput = s }
            , Cmd.none
            )

        StoreData ->
            ( { model | textinput = "" }
            , textInputToTable model.textinput |> Storage.addRows storage
            )

        DeleteAllData ->
            ( model, Storage.truncate )


textInputToTable : String -> List String
textInputToTable data =
    data |> String.split "\n"



-- VIEW


view : Storage -> Model -> View Msg
view storage model =
    { title = "Import"
    , body = [ Layout.layout "Import" <| showData storage model ]
    }


showData : Storage -> Model -> Element Msg
showData storage model =
    column [ spacing 20 ]
        [ Input.multiline [ width (fill |> maximum 600 |> minimum 450), height (px 200 |> maximum 400) ]
            { onChange = Textinput
            , text = model.textinput
            , placeholder = Just (placeholder [] (text "CSV Data"))
            , label = labelAbove [] (text "Insert CSV")
            , spellcheck = False
            }
        , Input.button Layout.style.button
            { onPress = Just StoreData
            , label = text "Add"
            }
        , el [] <| text <| (Dict.size storage.rawData |> String.fromInt) ++ " rows in the DB"
        , Input.button Layout.style.button
            { onPress = Just DeleteAllData
            , label = text "Delete everything"
            }
        ]
