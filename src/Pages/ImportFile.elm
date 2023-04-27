module Pages.ImportFile exposing (Model, Msg, page)

import CsvParser exposing (toEntries)
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, none, row, spacing, text, width)
import Element.Border as Border
import Element.Input as Input exposing (labelRight)
import File exposing (File)
import File.Select as Select
import Gen.Params.ImportFile exposing (Params)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Layout exposing (style)
import Page
import Pages.Bookings exposing (showData)
import Request
import Shared
import Storage exposing (Storage, hashData)
import Task exposing (Task)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    { hover : Bool
    , fileContents : List String
    , fileName : String
    , skipFirst : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model False [] "" False, Cmd.none )



-- UPDATE


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | Store
    | CheckFirst Bool


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        PickFile ->
            ( model
            , Select.file [ "*" ] GotFileName
            )

        GotFileName filename ->
            ( { model | hover = False }, readFile filename )

        GotFile name content ->
            ( { model | fileContents = readFileContents content, fileName = name }, Cmd.none )

        Store ->
            ( model, Storage.addRows storage model.fileContents )

        CheckFirst b ->
            ( { model | skipFirst = b }, Cmd.none )


readFile : File -> Cmd Msg
readFile file =
    Task.perform (GotFile <| File.name file) <| File.toString file


readFileContents : String -> List String
readFileContents content =
    List.filter (not << String.isEmpty) (String.split "\n" content)



-- VIEW


view : Storage -> Model -> View Msg
view _ model =
    { title = "Import File"
    , body =
        [ Layout.layout "Import File" <|
            if List.isEmpty model.fileContents then
                viewFilePicker model

            else
                viewFileContents model
        ]
    }



-- FILE PICKER


viewFilePicker : Model -> Element Msg
viewFilePicker model =
    column [ width fill, height fill, spacing Layout.size.m ]
        [ el [] (text "Import a CSV File")
        , el
            [ width fill
            , height fill
            , Border.dashed
            , Border.color <|
                if model.hover then
                    Layout.color.lightAccent

                else
                    Layout.color.darkAccent
            , Border.width Layout.size.xs
            , Border.rounded Layout.size.xl
            , onEvent "dragenter" (D.succeed DragEnter)
            , onEvent "dragover" (D.succeed DragEnter)
            , onEvent "dragleave" (D.succeed DragLeave)
            , onEvent "drop" fileDropDecoder
            ]
            (Input.button ([ centerX, centerY ] ++ Layout.style.button) { onPress = Just PickFile, label = text "Select File" })
        ]


fileDropDecoder : D.Decoder Msg
fileDropDecoder =
    D.map GotFileName (D.at [ "dataTransfer", "files" ] (D.oneOrMore (\a -> \_ -> a) File.decoder))


onEvent : String -> D.Decoder msg -> Attribute msg
onEvent event decoder =
    preventDefaultOn event (D.map (\msg -> ( msg, True )) decoder) |> Element.htmlAttribute



-- CSV Importer


viewFileContents : Model -> Element Msg
viewFileContents model =
    let
        dict =
            hashData model.fileContents

        csv =
            toEntries dict
    in
    column [ style.contentSpacing ]
        [ text ("Importing: " ++ model.fileName)
        , row []
            [ text "Import options"
            , Input.checkbox [] { label = labelRight [] (text "Skip first row"), icon = Input.defaultCheckbox, checked = model.skipFirst, onChange = CheckFirst }
            ]
        , Input.button style.button { onPress = Just Store, label = text "Store Data" }
        , showData Nothing csv
        ]
