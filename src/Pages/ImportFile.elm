module Pages.ImportFile exposing (Model, Msg, page)

import Csv exposing (Entry, Unparsed, allEntries)
import Dict
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, indexedTable, paddingXY, row, shrink, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelRight)
import File exposing (File)
import File.Select as Select
import Gen.Params.ImportFile exposing (Params)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Layout exposing (formatEuro, size, style)
import Maybe.Extra
import Page
import Request
import Result.Extra
import Shared
import Storage exposing (Storage, hashData, sha1)
import Task exposing (Task)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = ( initModel, Cmd.none )
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type State
    = Pick
    | PickHover
    | Show
    | Stored Int


type alias Model =
    { state : State
    , fileContents : List String
    , fileName : String
    , skipFirst : Bool
    }


initModel : Model
initModel =
    Model Pick [] "" False



-- UPDATE


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | Store
    | SkipFirst Bool


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        DragEnter ->
            ( { model | state = PickHover }, Cmd.none )

        DragLeave ->
            ( { model | state = Pick }, Cmd.none )

        PickFile ->
            ( model
            , Select.file [ "*" ] GotFileName
            )

        GotFileName filename ->
            ( { model | state = Pick }, readFile filename )

        GotFile name content ->
            ( { model | state = Show, fileContents = readFileContents content, fileName = name }, Cmd.none )

        Store ->
            ( { initModel | state = Stored (List.length model.fileContents - dropN model) }
            , List.drop (dropN model) model.fileContents
                |> List.map (\l -> ( sha1 l, l ))
                |> Dict.fromList
                |> Csv.validEntries
                |> Storage.addEntries storage
            )

        SkipFirst b ->
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
        (showStoreConfirmation model.state
            ++ [ el [] (text "Import a CSV File")
               , el
                    [ width fill
                    , height fill
                    , Border.dashed
                    , Border.color <|
                        if model.state == PickHover then
                            Layout.color.brightAccent

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
        )


fileDropDecoder : D.Decoder Msg
fileDropDecoder =
    D.map GotFileName (D.at [ "dataTransfer", "files" ] (D.oneOrMore (\a -> \_ -> a) File.decoder))


onEvent : String -> D.Decoder msg -> Attribute msg
onEvent event decoder =
    preventDefaultOn event (D.map (\msg -> ( msg, True )) decoder) |> Element.htmlAttribute


showStoreConfirmation : State -> List (Element msg)
showStoreConfirmation s =
    case s of
        Stored n ->
            [ text <| "Stored " ++ String.fromInt n ++ " rows in the DB" ]

        _ ->
            []



-- CSV Importer


viewFileContents : Model -> Element Msg
viewFileContents model =
    let
        csv =
            model.fileContents
                |> List.drop (dropN model)
                |> hashData
                |> allEntries
    in
    column [ style.contentSpacing ]
        ([ text ("Importing: " ++ model.fileName)
         , row []
            [ text "Import options:"
            , Input.checkbox [] { label = labelRight [] (text "Skip first row"), icon = Input.defaultCheckbox, checked = model.skipFirst, onChange = SkipFirst }
            ]
         , Input.button style.button { onPress = Just Store, label = text "Store Data" }
         ]
            ++ unreadableData csv
            ++ readableData csv
        )


dropN model =
    if model.skipFirst then
        1

    else
        0


unreadableData : List (Result Unparsed Entry) -> List (Element Msg)
unreadableData l =
    let
        unreadable =
            l
                |> List.map Result.Extra.error
                |> Maybe.Extra.values

        n =
            List.length unreadable
    in
    if n == 0 then
        []

    else
        [ text <| "There are " ++ String.fromInt n ++ " rows that can't be parsed:"
        , table [ spacing size.xs ]
            { data = unreadable
            , columns =
                [ { header = text "ID"
                  , width = shrink
                  , view = \e -> el [ Font.size Layout.size.s ] <| text e.id
                  }
                , { header = text "Line"
                  , width = shrink
                  , view = \e -> el [ Font.size Layout.size.m ] <| text e.text
                  }
                ]
            }
        ]


readableData : List (Result Unparsed Entry) -> List (Element Msg)
readableData l =
    let
        readable =
            l
                |> List.map Result.toMaybe
                |> Maybe.Extra.values

        n =
            List.length readable
    in
    if n == 0 then
        []

    else
        [ text <| "Importing " ++ String.fromInt n ++ " rows:"
        , indexedTable [ spacing size.xs ]
            { data = readable
            , columns =
                [ { header = text "Row"
                  , width = shrink
                  , view = \i _ -> textCell i <| String.fromInt (i + 1)
                  }
                , { header = text "Date"
                  , width = shrink
                  , view = \i e -> textCell i <| e.date
                  }
                , { header = text "Amount"
                  , width = shrink
                  , view = \i e -> formatEuro (cellstyle i) <| e.amount
                  }
                , { header = text "Description"
                  , width = shrink
                  , view = \i e -> textCell i <| e.description
                  }
                ]
            }
        ]


textCell i s =
    el (cellstyle i) <| text s


cellstyle : Int -> List (Attribute msg)
cellstyle _ =
    [ paddingXY 0 size.s ]
