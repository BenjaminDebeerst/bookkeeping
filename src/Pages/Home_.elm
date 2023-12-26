module Pages.Home_ exposing (Model, Msg, page)

import Components.Icons exposing (loader)
import Components.Layout as Layout exposing (color, size)
import Components.Notification as Notification exposing (Notification)
import Dict
import Element exposing (Element, el, fill, minimum, row, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Page
import Persistence.Data exposing (Data, decode, decodeJson, empty, encode, encodeJson)
import Persistence.Storage as Storage
import Request exposing (Request)
import Serialize exposing (Error(..))
import Shared exposing (Model(..))
import Task
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
    { notification : Notification Msg
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


emptyModel =
    { notification = Notification.None }



-- UPDATE


type FileFormat
    = String
    | Json


type Msg
    = InitDatabase
    | PickFile FileFormat
    | GotFileName FileFormat File
    | GotFileContent FileFormat String String
    | SaveDataBase FileFormat


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update sharedModel msg model =
    case msg of
        PickFile format ->
            ( model
            , Select.file [ "*" ] (\a -> GotFileName format a)
            )

        GotFileName format file ->
            ( { model | notification = Notification.Info [ loader [ Font.color color.black ] size.m, text "Loading..." ] }, readFile format file )

        GotFileContent format name content ->
            load format name content

        InitDatabase ->
            ( emptyModel, Storage.truncate )

        SaveDataBase format ->
            ( model, save format sharedModel )



-- MSG processing


readFile : FileFormat -> File -> Cmd Msg
readFile format file =
    Task.perform (GotFileContent format <| File.name file) <| File.toString file


load : FileFormat -> String -> String -> ( Model, Cmd Msg )
load format fileName content =
    case format of
        String ->
            ( { emptyModel | notification = Notification.Info [ text <| "Loaded " ++ fileName ] }
            , Storage.loadDatabase content
            )

        Json ->
            loadJson fileName content


loadJson : String -> String -> ( Model, Cmd Msg )
loadJson fileName content =
    let
        reencoded =
            content |> decodeJson |> Result.map encode
    in
    case reencoded of
        Ok data ->
            ( { emptyModel | notification = Notification.Info [ text <| "Loaded " ++ fileName ] }
            , Storage.loadDatabase data
            )

        Err e ->
            case e of
                CustomError err ->
                    ( { emptyModel | notification = Notification.Info [ text <| "Error transcoding json file: " ++ fileName ++ ": " ++ err ] }
                    , Cmd.none
                    )

                _ ->
                    ( { emptyModel | notification = Notification.Info [ text <| "Error transcoding json file: " ++ fileName ] }
                    , Cmd.none
                    )


save : FileFormat -> Shared.Model -> Cmd Msg
save format model =
    case model of
        None ->
            Cmd.none

        Loaded data ->
            case format of
                String ->
                    Download.string "bookkeeping.db" "text/plain" (encode data)

                Json ->
                    Download.string "bookkeeping.json" "text/plain" (encodeJson data)

        Problem _ ->
            Cmd.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view sharedModel model =
    Layout.page "Home" <|
        [ Notification.showNotification model.notification ]
            ++ (case sharedModel of
                    None ->
                        showStart

                    Loaded data ->
                        showDataSummary data

                    Problem e ->
                        showDataIssues e
               )


showStart : List (Element Msg)
showStart =
    [ el [] <| text "Welcome to Bookkeeping. What do you want to do?"
    , showActions [ Load, LoadJson, Init ]
    ]


showDataSummary : Data -> List (Element Msg)
showDataSummary data =
    let
        entries =
            data.rawEntries |> Dict.size |> String.fromInt

        accounts =
            data.accounts |> Dict.size |> String.fromInt

        categories =
            data.categories |> Dict.size |> String.fromInt
    in
    [ el [] <| text ([ "Database loaded. ", entries, " entries, ", accounts, " accounts, ", categories, " categories." ] |> String.concat)
    , showActions [ LoadOther, LoadOtherJson, Init, Save, SaveJson ]
    ]


showDataIssues : Error String -> List (Element Msg)
showDataIssues error =
    [ Notification.showNotification <| Notification.Error [ text "There was an issue loading the data from the DB!" ]
    , el [] <|
        text
            (case error of
                CustomError e ->
                    e

                DataCorrupted ->
                    "Data is corrupt. Was this a Bookkeeping database file?"

                SerializerOutOfDate ->
                    "Serializer out of date."
            )
    , showActions [ LoadOther, LoadOtherJson, Init ]
    ]


type Action
    = Load
    | LoadJson
    | LoadOther
    | LoadOtherJson
    | Init
    | Save
    | SaveJson


showActions : List Action -> Element Msg
showActions buttons =
    row
        [ width <| minimum 600 fill, Font.size size.m, spacing size.s ]
        (buttons
            |> List.map
                (\button ->
                    case button of
                        Load ->
                            Input.button Layout.style.button
                                { onPress = Just (PickFile String)
                                , label = text "Load a DB file."
                                }

                        LoadJson ->
                            Input.button Layout.style.button
                                { onPress = Just (PickFile Json)
                                , label = text "Load a JSON file."
                                }

                        LoadOther ->
                            Input.button Layout.style.button
                                { onPress = Just (PickFile String)
                                , label = text "Load another DB file"
                                }

                        LoadOtherJson ->
                            Input.button Layout.style.button
                                { onPress = Just (PickFile Json)
                                , label = text "Load another JSON file"
                                }

                        Init ->
                            Input.button Layout.style.button
                                { onPress = Just InitDatabase
                                , label = text "Initialize an empty DB"
                                }

                        Save ->
                            Input.button Layout.style.button
                                { onPress = Just (SaveDataBase String)
                                , label = text "Save DB"
                                }

                        SaveJson ->
                            Input.button Layout.style.button
                                { onPress = Just (SaveDataBase Json)
                                , label = text "Save JSON"
                                }
                )
        )
