module Pages.CsvImport exposing (Model, Msg, page)

import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (id, placeholder)
import Html.Events exposing (onClick, onInput)
import Navigation
import Page
import Request exposing (Request)
import SHA1
import Shared
import Storage exposing (CsvLine, Storage)
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


textInputToTable : String -> List CsvLine
textInputToTable data =
    data |> String.split "\n" |> List.map (\l -> ( sha1 l, l ))


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex |> String.slice 0 8



-- VIEW


view : Storage -> Model -> View Msg
view storage _ =
    { title = "Import"
    , body = [ showData storage ]
    }


showData : Storage -> Html Msg
showData storage =
    div []
        [ Navigation.view
        , div [ id "edit" ]
            [ div []
                [ textarea [ placeholder "Import CSV data", onInput Textinput ] []
                , div [] [ button [ onClick StoreData ] [ text "Add" ] ]
                ]
            ]
        , div [ id "data" ]
            [ text <| (List.length storage.rawData |> String.fromInt) ++ " rows in the DB"
            ]
        , div [] [ button [ onClick DeleteAllData ] [ text "Delete everything" ] ]
        ]
