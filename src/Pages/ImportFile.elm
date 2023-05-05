module Pages.ImportFile exposing (Model, Msg, page)

import Csv exposing (Unparsed, parseEntries)
import Dict
import Dropdown
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, indexedTable, padding, paddingXY, px, row, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelRight)
import File exposing (File)
import File.Select as Select
import Gen.Params.ImportFile exposing (Params)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Layout exposing (color, formatDate, formatEuro, size, style)
import Maybe.Extra
import Page
import Persistence.Data exposing (Account, Data, Entry)
import Persistence.Storage as Storage
import Request
import Result.Extra
import Shared
import Task exposing (Task)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = ( initModel, Cmd.none )
        , update = update shared
        , view = view shared
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
    , accountDropdownState : Dropdown.State Account
    , fileContents : List String
    , fileName : String
    , account : Maybe Account
    }


initModel : Model
initModel =
    Model Pick (Dropdown.init "") [] "" Nothing



-- UPDATE


type Msg
    = AccountPicked (Maybe Account)
    | AccountDropdownMsg (Dropdown.Msg Account)
    | PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | ChooseAccount Int
    | Store


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        AccountPicked option ->
            ( { model | account = option }, Cmd.none )

        AccountDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (dropdownConfig data) subMsg model model.accountDropdownState
            in
            ( { model | accountDropdownState = state }, cmd )

        DragEnter ->
            ( { model | state = PickHover }, Cmd.none )

        DragLeave ->
            ( { model | state = Pick }, Cmd.none )

        PickFile ->
            ( model
            , Select.file [ "*" ] GotFileName
            )

        GotFileName filename ->
            ( model, readFile filename )

        GotFile name content ->
            ( { model | state = Show, fileContents = readFileContents content, fileName = name }, Cmd.none )

        ChooseAccount id ->
            ( { model | account = Dict.get id data.accounts }, Cmd.none )

        Store ->
            ( { initModel | state = Stored (List.length model.fileContents) }
            , model.fileContents
                |> Csv.parseValidEntries
                |> Storage.addEntries data
            )


readFile : File -> Cmd Msg
readFile file =
    Task.perform (GotFile <| File.name file) <| File.toString file


readFileContents : String -> List String
readFileContents content =
    List.filter (not << String.isEmpty) (String.split "\n" content)



-- VIEW


view : Data -> Model -> View Msg
view data model =
    { title = "Import File"
    , body =
        [ Layout.layout "Import File" <|
            if List.isEmpty model.fileContents then
                viewFilePicker data model

            else
                viewFileContents data model
        ]
    }



-- Account Selector


viewAccountSelector : Data -> Model -> Element Msg
viewAccountSelector data model =
    Dropdown.view (dropdownConfig data) model model.accountDropdownState
        |> el []


dropdownConfig : Data -> Dropdown.Config Account Msg Model
dropdownConfig data =
    let
        itemToPrompt item =
            text item.name

        itemToElement selected highlighted item =
            text item.name

        accounts =
            Dict.values data.accounts
    in
    Dropdown.basic
        { itemsFromModel = always accounts
        , selectionFromModel = \m -> m.account
        , dropdownMsg = AccountDropdownMsg
        , onSelectMsg = AccountPicked
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        }



-- FILE PICKER


viewFilePicker : Data -> Model -> Element Msg
viewFilePicker data model =
    column [ width fill, height fill, spacing Layout.size.m ]
        (showStoreConfirmation model.state
            ++ [ viewAccountSelector data model ]
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


viewFileContents : Data -> Model -> Element Msg
viewFileContents data model =
    let
        csv =
            parseEntries model.fileContents
    in
    column [ style.contentSpacing ]
        ([ el [ Font.size size.m ] <| text ("Importing file: " ++ model.fileName) ]
            ++ [ viewAccountSelector data model ]
            ++ unreadableData csv
            ++ readableData csv
            ++ [ Input.button style.button { onPress = Just Store, label = text "Import Data" } ]
        )


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
                  , view = \e -> el [ Font.size Layout.size.s ] <| text <| String.slice 0 8 e.id
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
        [ text <| "The following " ++ String.fromInt n ++ " rows were successfully parsed: "
        , indexedTable [ spacing size.xs ]
            { data = readable
            , columns =
                [ { header = text "Date"
                  , width = shrink
                  , view = \i e -> textCell i <| formatDate e.date
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
