module Pages.ImportFile exposing (Model, Msg, page)

import Components.Icons exposing (checkMark, copy, database, folderPlus, warnTriangle)
import Components.Layout as Layout exposing (color, formatDate, formatEuro, size, style, tooltip, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Csv.Decode as Decode
import Dict exposing (Dict)
import Element exposing (Attribute, Element, IndexedColumn, alignLeft, alignRight, below, centerX, centerY, el, fill, height, indexedTable, padding, paddingEach, paddingXY, row, shrink, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Gen.Params.ImportFile exposing (Params)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Page
import Parser
import Persistence.Data as Shared exposing (Account, Category, Data, ImportProfile, RawEntry, rawEntry, sha1)
import Persistence.Storage as Storage
import Processing.CategoryParser exposing (categoryShortNameOnly)
import Processing.CsvParser as CsvParser exposing (ParsedRow)
import Processing.Model exposing (getCategoryByShort)
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
    | Abort
    | Store ImportProfile (List String) (List AnnotatedRow)


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

        Abort ->
            ( initModel (Loaded data), Cmd.none )

        Store profile newCats newRows ->
            case model.account of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedAccount ->
                    let
                        dataWithNewCategories =
                            newCats
                                |> List.map (\short -> Category 0 (short ++ " (auto)") short)
                                |> (\cs -> Storage.addCategories cs data)

                        categories =
                            Dict.values dataWithNewCategories.categories

                        newEntries =
                            newRows
                                |> List.map .parsedRow
                                |> List.map
                                    (\row ->
                                        rawEntry
                                            selectedAccount.id
                                            profile.id
                                            row.rawLine
                                            row.date
                                            row.amount
                                            row.description
                                            (Maybe.andThen (getCategoryByShort categories) row.category)
                                    )

                        newData =
                            dataWithNewCategories |> Storage.addEntries newEntries

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
        ++ viewFileData
            (\s -> Dict.member s data.rawEntries)
            (getCategoryByShort (Dict.values data.categories))
            model.importProfile
            content


viewFileData : (String -> Bool) -> (String -> Maybe Category) -> Maybe ImportProfile -> String -> List (Element Msg)
viewFileData entryIdExists findCategory importProfile csvFileContent =
    case importProfile |> Maybe.map (\p -> ( p, CsvParser.parse p csvFileContent )) of
        Nothing ->
            [ text "Choose an import profile" ]

        Just ( profile, Ok rows ) ->
            showParsingSuccess
                profile
                rows
                entryIdExists
                findCategory

        Just ( _, Err errors ) ->
            showParsingErrors csvFileContent errors


showParsingErrors csvFileContent errors =
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
    , indexedTable T.tableStyle
        { data = rows
        , columns = [ T.fullStyledColumn Element.none (el [ Font.size Layout.size.m ] << text) ]
        }
    ]


showParsingSuccess : ImportProfile -> List ParsedRow -> (String -> Bool) -> (String -> Maybe Category) -> List (Element Msg)
showParsingSuccess profile rows entryIdExists findCategory =
    let
        n =
            List.length rows

        duplicates =
            findDuplicateRows rows

        ( annotatedRows, overlap ) =
            annotateRows rows duplicates entryIdExists findCategory

        existingEntriesWarning =
            if overlap == 0 then
                []

            else
                [ row []
                    [ warnTriangle [ padding size.xs, Font.color color.red ] size.l
                    , text <| "This CSV contains " ++ String.fromInt overlap ++ " rows that have already been imported."
                    ]
                ]

        duplicatesWarning =
            if Dict.size duplicates == 0 then
                []

            else
                [ row []
                    [ warnTriangle [ padding size.xs, Font.color color.red ] size.l
                    , text <| "This CSV contains " ++ (String.fromInt <| Dict.size duplicates) ++ " distinct duplicated rows. Duplicate rows will be imported only once. Does your CSV really contain duplicates? Or are individual bank statements not sufficiently distinct?"
                    ]
                ]

        newCategories =
            annotatedRows
                |> List.Extra.uniqueBy (.parsedRow >> .category)
                |> List.filter (.category >> Maybe.Extra.isNothing)
                |> List.map (.parsedRow >> .category)
                |> Maybe.Extra.values
                |> List.sort

        categoryCreationWarning =
            if List.length newCategories == 0 then
                []

            else
                [ row []
                    [ warnTriangle [ padding size.xs, Font.color color.red ] size.l
                    , text <| (String.fromInt <| List.length newCategories) ++ " yet unknown categories were found in the CSV. They shall be created upon import: " ++ String.join ", " newCategories
                    ]
                ]
    in
    if n == 0 then
        [ text "The CSV file was empty. There's nothing to do." ]

    else
        existingEntriesWarning
            ++ duplicatesWarning
            ++ categoryCreationWarning
            ++ [ row [ spacing size.s ]
                    [ Input.button style.button { onPress = Just Abort, label = text "Abort" }
                    , Input.button style.button { onPress = Just (Store profile newCategories annotatedRows), label = text "Import Data" }
                    ]
               , text <| "Parsed " ++ String.fromInt n ++ " rows. "
               , indexedTable T.tableStyle
                    { data = annotatedRows
                    , columns =
                        [ T.styledColumn "Info" <| annotations
                        , T.textColumn "Date" (.parsedRow >> .date >> formatDate)
                        , T.styledColumn "Amount" (.parsedRow >> .amount >> formatEuro)
                        ]
                            ++ categoryColumn (Maybe.Extra.isJust profile.categoryField)
                            ++ [ T.textColumn "Description" (.parsedRow >> .description) ]
                    }
               ]


annotations : AnnotatedRow -> Element msg
annotations annotatedRow =
    let
        ants =
            [ if annotatedRow.duplicates > 1 then
                Just <|
                    copy
                        [ Font.color color.black
                        , tooltip below ("This row was found " ++ String.fromInt annotatedRow.duplicates ++ " times.")
                        ]
                        size.m

              else
                Nothing
            , if annotatedRow.alreadyImported then
                Just
                    (database
                        [ Font.color color.black
                        , tooltip below "This row is already present in the DB."
                        ]
                        size.m
                    )

              else
                Nothing
            ]
                |> Maybe.Extra.values
    in
    row [] ants


type alias AnnotatedRow =
    { parsedRow : ParsedRow
    , duplicates : Int
    , alreadyImported : Bool
    , category : Maybe Category
    }


annotateRows : List ParsedRow -> Dict String Int -> (String -> Bool) -> (String -> Maybe Category) -> ( List AnnotatedRow, Int )
annotateRows parsedRows duplicates entryIdExists categoryLookup =
    let
        ( annotatedRows, existingN ) =
            List.foldr
                (\row ( rows, n ) ->
                    let
                        rowId =
                            sha1 row.rawLine

                        exists =
                            entryIdExists rowId

                        m =
                            if exists then
                                n + 1

                            else
                                n

                        dups =
                            Dict.get rowId duplicates |> Maybe.withDefault 1

                        cat =
                            row.category
                                |> Maybe.map String.trim
                                |> Maybe.andThen categoryLookup
                    in
                    ( AnnotatedRow row dups exists cat :: rows, m )
                )
                ( [], 0 )
                parsedRows
    in
    ( annotatedRows, existingN )


findDuplicateRows : List ParsedRow -> Dict String Int
findDuplicateRows parsedRows =
    parsedRows
        |> List.foldl
            (\row dict ->
                Dict.update (sha1 row.rawLine)
                    (\val ->
                        case val of
                            Just i ->
                                Just (i + 1)

                            Nothing ->
                                Just 1
                    )
                    dict
            )
            Dict.empty
        |> Dict.filter (\_ v -> v > 1)


categoryColumn : Bool -> List (IndexedColumn AnnotatedRow msg)
categoryColumn show =
    if not show then
        []

    else
        [ T.styledColumn "Category"
            (\annotatedRow ->
                case annotatedRow.category of
                    Nothing ->
                        case annotatedRow.parsedRow.category of
                            Nothing ->
                                Element.none

                            Just newCat ->
                                if String.trim newCat == "" then
                                    Element.none

                                else
                                    case Parser.run categoryShortNameOnly newCat of
                                        Ok validShortName ->
                                            row [ width fill ]
                                                [ el [ alignLeft ] <| text validShortName
                                                , folderPlus [ padding 0, Font.color color.darkAccent, alignRight, tooltip below "New category, will be created upon import" ] size.m
                                                ]

                                        Err _ ->
                                            row [ width fill ]
                                                [ el [ alignLeft ] <| text newCat
                                                , warnTriangle [ padding 0, Font.color color.red, alignRight, tooltip below <| "Cannot create category, invalid category shortname." ] size.m
                                                ]

                    Just c ->
                        row [ width fill ]
                            [ el [ alignLeft ] <| text c.name
                            , checkMark [ padding 0, Font.color color.darkAccent, alignRight, tooltip below "Known Category" ] size.m
                            ]
            )
        ]
