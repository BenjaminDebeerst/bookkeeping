module Pages.ImportFile exposing (Model, Msg, page)

import Components.Layout as Layout exposing (formatDate, formatEuro, size, style, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Csv.Decode as Decode
import Dict exposing (Dict)
import Element exposing (Attribute, Element, centerX, centerY, el, fill, height, indexedTable, paddingEach, paddingXY, shrink, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Gen.Params.ImportFile exposing (Params)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Page
import Persistence.Data as Shared exposing (Account, Data, ImportProfile, RawEntry, rawEntry)
import Persistence.Storage as Storage
import Processing.CsvParser as CsvParser
import Request
import Shared exposing (Model(..))
import Task exposing (Task)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = ( initModel shared, Cmd.none )
        , update = updateOrRedirectOnError shared req update
        , view = viewDataOnly shared view
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
    , fileContents : Maybe String
    , fileName : Maybe String
    , importProfile : Maybe ImportProfile
    , account : Maybe Account
    }


initModel : Shared.Model -> Model
initModel shared =
    let
        db =
            Shared.justData shared

        acc =
            db |> Maybe.andThen (\d -> selectIfUnique d .accounts)

        profile =
            db |> Maybe.andThen (\d -> selectIfUnique d .importProfiles)
    in
    Model Pick Nothing Nothing profile acc


selectIfUnique : Shared.Data -> (Shared.Data -> Dict a b) -> Maybe b
selectIfUnique data accessor =
    if Dict.size (accessor data) == 1 then
        accessor data |> Dict.values |> List.head

    else
        Nothing



-- UPDATE


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | ChooseImportProfile ImportProfile
    | ChooseAccount Account
    | Store


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
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
            ( model, readFile filename )

        GotFile name content ->
            ( { model | state = Show, fileContents = Just content, fileName = Just name }, Cmd.none )

        ChooseImportProfile profile ->
            ( { model | importProfile = Just profile }, Cmd.none )

        ChooseAccount account ->
            ( { model | account = Just account }, Cmd.none )

        Store ->
            let
                newEntries =
                    case
                        Maybe.map3
                            (\acc profile content -> ( acc, profile, CsvParser.parse profile content ))
                            model.account
                            model.importProfile
                            model.fileContents
                    of
                        Nothing ->
                            []

                        Just ( _, _, Err _ ) ->
                            []

                        Just ( account, profile, Ok lines ) ->
                            lines
                                |> List.map
                                    (\row ->
                                        rawEntry
                                            account.id
                                            profile.id
                                            row.rawLine
                                            row.date
                                            row.amount
                                            row.description
                                    )

                newData =
                    data |> Storage.addEntries newEntries

                newModel =
                    initModel (Loaded data)
            in
            ( { newModel | state = Stored (List.length newEntries) }
            , Storage.store newData
            )


readFile : File -> Cmd Msg
readFile file =
    Task.perform (GotFile <| File.name file) <| File.toString file



-- VIEW


view : Data -> Model -> View Msg
view data model =
    Layout.page "Import File" <|
        case model.fileContents of
            Nothing ->
                viewFilePicker data model

            Just content ->
                viewFileContents data model content


viewImportSelectors : Data -> Model -> List (Element Msg)
viewImportSelectors data model =
    [ Input.radioRow []
        { onChange = ChooseImportProfile
        , selected = model.importProfile
        , label = Input.labelLeft [ paddingXY size.m 0 ] <| text "Choose import profile: "
        , options =
            Dict.values data.importProfiles
                |> List.map
                    (\p ->
                        Input.option p (el [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text p.name)
                    )
        }
    , Input.radioRow []
        { onChange = ChooseAccount
        , selected = model.account
        , label = Input.labelLeft [ paddingXY size.m 0 ] <| text "Choose account: "
        , options =
            Dict.values data.accounts
                |> List.map
                    (\a ->
                        Input.option a (el [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text a.name)
                    )
        }
    ]



-- FILE PICKER


viewFilePicker : Data -> Model -> List (Element Msg)
viewFilePicker data model =
    showStoreConfirmation model.state
        ++ viewImportSelectors data model
        ++ [ el
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


viewFileContents : Data -> Model -> String -> List (Element Msg)
viewFileContents data model content =
    [ el [ Font.size size.m ] <| text ("Importing file: " ++ Maybe.withDefault "None" model.fileName) ]
        ++ viewImportSelectors data model
        ++ viewFileData model content


viewFileData : Model -> String -> List (Element Msg)
viewFileData model csvFileContent =
    case model.importProfile |> Maybe.map (\p -> CsvParser.parse p csvFileContent) of
        Nothing ->
            [ text "Choose an import profile" ]

        Just (Ok rows) ->
            let
                n =
                    List.length rows
            in
            if n == 0 then
                []

            else
                [ Input.button style.button { onPress = Just Store, label = text "Import Data" }
                , text <| "The following " ++ String.fromInt n ++ " rows were successfully parsed: "
                , indexedTable T.tableStyle
                    { data = rows
                    , columns =
                        [ T.textColumn "Date" (.date >> formatDate)
                        , T.styledColumn "Amount" (.amount >> formatEuro [])
                        , T.textColumn "Description" .description
                        ]
                    }
                ]

        Just (Err errors) ->
            let
                rows =
                    csvFileContent
                        |> String.split "\n"
                        |> List.map String.trim
                        |> List.take 5
            in
            [ errors
                |> Decode.errorToString
                |> text
            , text "This is how the first few rows in the CSV look like:"
            , table [ spacing size.xs ]
                { data = rows
                , columns =
                    [ { header = Element.none
                      , width = shrink
                      , view = \line -> el [ Font.size Layout.size.m ] <| text line
                      }
                    ]
                }
            ]
