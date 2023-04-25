module Pages.Home_ exposing (Model, Msg, page)

import Element exposing (Element, centerX, centerY, column, el, fill, htmlAttribute, layout, maximum, padding, spacing, text, width)
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, placeholder)
import Html.Attributes
import Layout
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
    , body = [ Layout.layout "Home" (showData storage model) ]
    }


showData : Storage -> Model -> Element Msg
showData storage model =
    column [ spacing 20 ]
        ([ el [] <| text ("Currently, the DB has " ++ (String.fromInt <| List.length <| storage.rawData) ++ " entries. You can start over or load a database.")
         , Input.multiline [ width <| maximum 450 fill ]
            { onChange = TextInput
            , text = model.textinput
            , placeholder = Just (placeholder [] (text "Database string"))
            , label = labelAbove [] (text "Label")
            , spellcheck = False
            }
         , Input.button []
            { onPress = Just LoadData
            , label = text "Load"
            }
         , Input.button []
            { onPress = Just StartFromScratch
            , label = text "Start with a clean slate (delete the current DB)"
            }
         , Input.button []
            { onPress = Just SaveData
            , label = text "Save DB"
            }
         ]
            ++ showSave model
        )


showSave : Model -> List (Element msg)
showSave model =
    case model.dbString of
        Nothing ->
            []

        Just s ->
            [ el [] (text "Save the follwing string in a file to store the DB:")
            , el [ Html.Attributes.style "word-wrap" "break-word" |> htmlAttribute ] (text s)
            ]
