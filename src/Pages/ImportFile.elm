module Pages.ImportFile exposing (Model, Msg, page)

import Components.Layout as Layout exposing (formatDate, formatEuro, size, style, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Csv.Decode as Decode exposing (Error(..))
import Csv.Parser as Parser exposing (Problem(..))
import Dict exposing (Dict)
import Element exposing (Attribute, Element, centerX, centerY, el, fill, height, indexedTable, paddingEach, paddingXY, text, width)
import Element.Border as Border
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Gen.Params.ImportFile exposing (Params)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Page
import Persistence.Data as Shared exposing (Account, Data, ImportProfile, RawEntry, rawEntry)
import Persistence.Storage as Storage
import Processing.CsvParser as CsvParser exposing (ParsedRow)
import Request
import Shared exposing (Model(..))
import Task exposing (Task)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = ( Pick, Cmd.none )
        , update = updateOrRedirectOnError shared req update
        , view = viewDataOnly shared view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type Model
    = Pick
    | PickHover
    | Show String String (Maybe ImportProfile)
    | Stored Int



-- UPDATE


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | ChooseImportProfile ImportProfile
    | Store Account ImportProfile (List ParsedRow)


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        DragEnter ->
            ( PickHover, Cmd.none )

        DragLeave ->
            ( Pick, Cmd.none )

        PickFile ->
            ( model
            , Select.file [ "*" ] GotFileName
            )

        GotFileName filename ->
            ( model, readFile filename )

        GotFile name content ->
            ( Show name content Nothing, Cmd.none )

        ChooseImportProfile profile ->
            case model of
                Show name content _ ->
                    ( Show name content (Just profile), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Store account profile lines ->
            let
                newEntries =
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
            in
            ( Stored (List.length lines)
            , Storage.store newData
            )


readFile : File -> Cmd Msg
readFile file =
    Task.perform (GotFile <| File.name file) <| File.toString file



-- VIEW


view : Data -> Model -> View Msg
view data model =
    Layout.page "Import CSV File" <|
        case model of
            Show fileName content profile ->
                viewFileContents data profile fileName content

            _ ->
                viewFilePicker data model



-- FILE PICKER


viewFilePicker : Data -> Model -> List (Element Msg)
viewFilePicker _ model =
    showStoreConfirmation model
        ++ [ el
                [ width fill
                , height fill
                , Border.dashed
                , Border.color <|
                    if model == PickHover then
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


showStoreConfirmation : Model -> List (Element msg)
showStoreConfirmation s =
    case s of
        Stored n ->
            [ text <| "Stored " ++ String.fromInt n ++ " rows in the DB" ]

        _ ->
            []



-- CSV Importer


viewFileContents : Data -> Maybe ImportProfile -> String -> String -> List (Element Msg)
viewFileContents data profile filename content =
    case profile of
        Nothing ->
            preview data Nothing filename content

        Just p ->
            preview data (Just p) filename content
                ++ parseFile data p content


parseFile : Data -> ImportProfile -> String -> List (Element Msg)
parseFile data profile csvFileContent =
    case CsvParser.parse profile csvFileContent of
        Err error ->
            [ text "Could not parse the given file with the selected profile.", text (errorToString error) ]

        Ok rows ->
            let
                n =
                    List.length rows
            in
            if n == 0 then
                []

            else
                [ text "Import to account: " ]
                    ++ (Dict.values
                            data.accounts
                            |> List.map (\a -> Input.button style.button { onPress = Just (Store a profile rows), label = text a.name })
                       )
                    ++ [ text <| "Successfully parsed " ++ String.fromInt n ++ " rows:"
                       , indexedTable T.tableStyle
                            { data = rows
                            , columns =
                                [ T.textColumn "Date" (.date >> formatDate)
                                , T.styledColumn "Amount" (.amount >> formatEuro)
                                , T.textColumn "Description" .description
                                ]
                            }
                       ]


errorToString : Error -> String
errorToString error =
    case error of
        DecodingErrors list ->
            let
                n =
                    List.length list

                prefix =
                    String.join "" [ "There were ", String.fromInt n, " errors parsing the CSV. Showing the first few: " ]

                errs =
                    list |> List.take 10 |> List.map (\e -> DecodingErrors [ e ]) |> List.map Decode.errorToString
            in
            [ prefix ] ++ errs |> String.join "\n"

        other ->
            other |> Decode.errorToString


preview : Data -> Maybe ImportProfile -> String -> String -> List (Element Msg)
preview data selectedProfile filename contents =
    case Parser.parse { fieldSeparator = ',' } contents of
        Ok csv ->
            [ text <| "Loaded " ++ (csv |> List.length |> String.fromInt) ++ " rows from " ++ filename ++ ". This is how the raw data looks like:"
            , previewTable csv
            , Input.radioRow []
                { onChange = ChooseImportProfile
                , selected = selectedProfile
                , label = Input.labelLeft [ paddingXY size.m 0 ] <| text "Choose import profile: "
                , options =
                    Dict.values data.importProfiles
                        |> List.map
                            (\p ->
                                Input.option p (el [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text p.name)
                            )
                }
            ]

        Err problem ->
            case problem of
                SourceEndedWithoutClosingQuote _ ->
                    [ text "Problem parsing CSV file: Unclosed quoted text field." ]

                AdditionalCharactersAfterClosingQuote _ ->
                    [ text "Problem parsing CSV file: Unexpected text after quoted string before filed separator." ]


previewTable : List (List String) -> Element msg
previewTable csv =
    let
        headers =
            csv |> List.head |> Maybe.withDefault []

        data =
            csv |> List.drop 1 |> List.take 5
    in
    indexedTable T.tableStyle
        { data = data
        , columns =
            headers
                |> List.indexedMap
                    (\i title -> T.textColumn title (List.drop i >> List.head >> Maybe.withDefault "n/a"))
        }
