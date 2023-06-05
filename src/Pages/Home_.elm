module Pages.Home_ exposing (Model, Msg, page)

import Components.Layout as Layout
import Dict
import Element exposing (Element, el, fill, height, maximum, px, text, width)
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, placeholder)
import Page
import Persistence.Data exposing (Data, encode)
import Persistence.Storage as Storage
import Request exposing (Request)
import Serialize exposing (Error(..))
import Shared exposing (Model(..))
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


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update sharedModel msg model =
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
            case sharedModel of
                Loaded data ->
                    ( { model | dbString = Just (encode data) }, Cmd.none )

                Problem _ ->
                    ( { model | dbString = Nothing }, Cmd.none )



-- VIEW


view : Shared.Model -> Model -> View Msg
view sharedModel model =
    Layout.page "Home" <|
        case sharedModel of
            Loaded data ->
                showDataSummary data model

            Problem e ->
                showDataIssues e


showDataSummary : Data -> Model -> List (Element Msg)
showDataSummary data model =
    [ el [] <| text ("Currently, the DB has " ++ (String.fromInt <| Dict.size <| data.rawEntries) ++ " entries. You can start over or load a database.")
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


showSave : Model -> List (Element msg)
showSave model =
    case model.dbString of
        Nothing ->
            []

        Just s ->
            [ el [] (text "Save the follwing string in a file to store the DB:")
            , el [ Font.size Layout.size.s ] (text s)
            ]


showDataIssues : Error String -> List (Element Msg)
showDataIssues error =
    [ el [] <| text "There was an issue loading the data from the DB!"
    , el [] <|
        text
            (case error of
                CustomError e ->
                    e

                DataCorrupted ->
                    "Data is corrupt. Did you introduce a breaking codec change?"

                SerializerOutOfDate ->
                    "Serializer out of date."
            )
    ]
