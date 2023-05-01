module Pages.Home_ exposing (Model, Msg, page)

import Dict
import Element exposing (Element, column, el, fill, height, maximum, px, spacing, text, width)
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, placeholder)
import Layout
import Page
import Persistence.Data exposing (Data, encode)
import Persistence.Storage as Storage
import Request exposing (Request)
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared.data
        , view = view shared.data
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


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
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
            ( { model | dbString = Just (encode data) }, Cmd.none )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    { title = "Home"
    , body = [ Layout.layout "Home" (showData data model) ]
    }


showData : Data -> Model -> Element Msg
showData data model =
    column [ spacing 20 ]
        ([ el [] <| text ("Currently, the DB has " ++ (String.fromInt <| Dict.size <| data.bookEntries) ++ " entries. You can start over or load a database.")
         , Input.multiline [ width <| maximum 600 fill, height <| maximum 400 <| px 200 ]
            { onChange = TextInput
            , text = model.textinput
            , placeholder = Just (placeholder [] (text "Database string"))
            , label = labelAbove [] (text "Database to load")
            , spellcheck = False
            }
         , Input.button Layout.style.button
            { onPress = Just LoadData
            , label = text "Load"
            }
         , Input.button Layout.style.button
            { onPress = Just StartFromScratch
            , label = text "Start with a clean slate (delete the current DB)"
            }
         , Input.button Layout.style.button
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
            , el [ Font.size Layout.size.s ] (text s)
            ]
