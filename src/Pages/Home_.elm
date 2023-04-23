module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (id, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Navigation
import Page
import Request exposing (Request)
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
    , dbString : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


emptyModel =
    { textinput = "", dbString = Nothing }



-- UPDATE


type Msg
    = TextInput String
    | LoadData
    | StartFromScratch
    | SaveData


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        TextInput s ->
            ( { model | textinput = s }
            , Cmd.none
            )

        LoadData ->
            ( emptyModel
            , Storage.loadDatabase model.textinput
            )

        StartFromScratch ->
            ( emptyModel, Storage.truncate )

        SaveData ->
            ( { model | dbString = Just (Storage.encode storage) }, Cmd.none )



-- VIEW


view : Storage -> Model -> View Msg
view storage model =
    { title = "Home"
    , body = [ showData storage model ]
    }


showData : Storage -> Model -> Html Msg
showData storage model =
    div []
        [ Navigation.view
        , div [] [ text ("Currently, the DB has " ++ (String.fromInt <| List.length <| storage.rawData) ++ " entries. You can start over or load a database.") ]
        , div [ id "edit" ]
            [ div []
                [ textarea [ placeholder "Database string", onInput TextInput ] []
                , div [] [ button [ onClick LoadData ] [ text "Load" ] ]
                ]
            ]
        , div [] [ button [ onClick StartFromScratch ] [ text "Start with a clean slate (delete the current DB)" ] ]
        , showSave model
        ]


showSave model =
    div []
        ([ button [ onClick SaveData ] [ text "Save DB" ] ]
            ++ (case model.dbString of
                    Nothing ->
                        []

                    Just s ->
                        [ div [] [ text "Save the follwing string in a file to store the DB:" ]
                        , div [ style "word-wrap" "break-word", style "font-size" "8pt" ] [ text s ]
                        ]
               )
        )
