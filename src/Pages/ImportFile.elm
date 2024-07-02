module Pages.ImportFile exposing (Model, Msg, page)

import Array exposing (Array)
import Components.Icons exposing (checkMark, folderPlus, infoMark, plusSquare, warnTriangle)
import Components.Input exposing (brightButton, button)
import Components.Notification as Notification exposing (Notification)
import Components.Table as T
import Components.Tooltip exposing (tooltip)
import Config exposing (color, size, style)
import Csv.Decode as Decode exposing (Error(..))
import Csv.Parser as Parser exposing (Problem(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Attribute, Color, Element, IndexedColumn, alignRight, centerX, centerY, column, el, fill, height, indexedTable, onRight, padding, paddingEach, paddingXY, px, row, spacing, text, width)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelLeft, labelRight, placeholder)
import File exposing (File)
import File.Select as Select
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Layouts
import List.Extra
import Maybe.Extra
import Page exposing (Page)
import Parser
import Persistence.Account exposing (Account)
import Persistence.Category as Category exposing (Category, category)
import Persistence.Data exposing (Data)
import Persistence.ImportProfile exposing (DateFormat(..), ImportProfile, importProfile)
import Persistence.RawEntry exposing (rawEntry)
import Persistence.Storage as Storage
import Processing.CategorizationRules exposing (applyAllCategorizationRules)
import Processing.CategoryParser as CategoryParser
import Processing.CsvParser as CsvParser exposing (ParsedRow, toDate)
import Processing.Model exposing (getCategoryByShort)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Task
import Time.Date
import Util.Formats exposing (formatDate, formatEuro)
import Util.Layout exposing (dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> ( initialModel, Effect.none )
        , update = dataUpdate shared update
        , view = dataView shared "Import CSV File" view
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Tabs { dataSummary = dataSummary shared })



-- MODEL


initialModel : Model
initialModel =
    { notification = Notification.None, state = SelectFile False }


type alias Model =
    { notification : Notification Msg
    , state : State
    }


type State
    = SelectFile Bool
    | Preview SelectedFile ParsedRawCsv
    | CreateParser SelectedFile ParsedRawCsv ParserConfig
    | Parsed SelectedFile ParsedFile


type alias ParserConfig =
    { separator : Maybe Char
    , dateColumn : Maybe Int
    , dateFormat : Maybe DateFormat
    , amountColumn : Maybe Int
    , descriptionColumns : List Int
    , categoryColumn : Maybe Int
    , selected : Maybe Int
    , name : String
    }


emptyParserConfig =
    ParserConfig Nothing Nothing Nothing Nothing [] Nothing Nothing ""


type alias ParsedRawCsv =
    Result Problem (List (List String))


type alias SelectedFile =
    { name : String
    , content : String
    }


type alias ParsedFile =
    { importProfile : ImportProfile
    , parsedRows : Result Error (List ParsedRow)
    , importFilter : ImportFilter
    }


type alias ImportFilter =
    { doFilter : Bool
    , minDate : String
    , maxDate : String
    }


emptyFilter =
    ImportFilter False "" ""



-- UPDATE


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | ChooseImportProfile ImportProfile
    | CustomParser CustomParserMsg
    | Filter Bool String String
    | Abort
    | Store Account ImportProfile (List String) (List ParsedRow)


type CustomParserMsg
    = StartCustomParser
    | SplitChar Char
    | Select Int
    | DateColumn Int
    | DateFormat DateFormat
    | AmountColumn Int
    | AddDescrColumn Int
    | RemoveDescrColumn Int
    | CategoryColumn Int
    | Name String
    | Save ImportProfile
    | AbortCustomParser


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        DragEnter ->
            ( { model | state = SelectFile True }, Effect.none )

        DragLeave ->
            ( { model | state = SelectFile False }, Effect.none )

        PickFile ->
            ( model
            , Select.file [ "*" ] GotFileName |> Effect.sendCmd
            )

        GotFileName filename ->
            ( model, readFile filename |> Effect.sendCmd )

        GotFile name content ->
            ( { model | state = Preview (SelectedFile name content) (parseFileForPreview Nothing content) }, Effect.none )

        ChooseImportProfile profile ->
            case model.state of
                Preview file _ ->
                    ( { model | state = Parsed file (parseFile file profile emptyFilter) }
                    , Effect.none
                    )

                Parsed file parsed ->
                    ( { model | state = Parsed file (parseFile file profile parsed.importFilter) }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        Filter on start end ->
            case model.state of
                Parsed file parsed ->
                    ( { model | state = Parsed file { parsed | importFilter = ImportFilter on start end } }, Effect.none )

                _ ->
                    ( model, Effect.none )

        CustomParser parserMsg ->
            updateCustomParser data parserMsg model

        Abort ->
            ( initialModel, Effect.none )

        Store account profile newCats lines ->
            let
                dataWithNewCategories =
                    newCats
                        |> List.map (\name -> category 0 (name ++ " (auto)") name Category.Expense [])
                        |> (\cs -> Storage.addCategories cs data)

                categories =
                    Dict.values dataWithNewCategories.categories

                categorizationRules =
                    applyAllCategorizationRules data

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
                                    (getCategoryForParsedRow categorizationRules categories row)
                            )

                newData =
                    dataWithNewCategories |> Storage.addEntries newEntries
            in
            ( { notification = storeConfirmation (List.length newEntries), state = SelectFile False }
            , newData |> Effect.store
            )


updateCustomParser : Data -> CustomParserMsg -> Model -> ( Model, Effect msg )
updateCustomParser data customParserMsg model =
    case model.state of
        Parsed file _ ->
            ( { model | state = CreateParser file (parseFileForPreview Nothing file.content) emptyParserConfig }, Effect.none )

        Preview file csv ->
            ( { model | state = CreateParser file csv emptyParserConfig }, Effect.none )

        CreateParser file csv parserConfig ->
            case customParserMsg of
                StartCustomParser ->
                    ( model, Effect.none )

                SplitChar char ->
                    ( { model | state = CreateParser file (parseFileForPreview (Just char) file.content) { parserConfig | separator = Just char, selected = Nothing } }, Effect.none )

                Select i ->
                    ( { model | state = CreateParser file csv { parserConfig | selected = Just i } }, Effect.none )

                DateColumn i ->
                    ( { model | state = CreateParser file csv { parserConfig | dateColumn = Just i } }, Effect.none )

                DateFormat format ->
                    ( { model | state = CreateParser file csv { parserConfig | dateFormat = Just format, selected = Nothing } }, Effect.none )

                AmountColumn i ->
                    ( { model | state = CreateParser file csv { parserConfig | amountColumn = Just i, selected = Nothing } }, Effect.none )

                AddDescrColumn i ->
                    ( { model | state = CreateParser file csv { parserConfig | descriptionColumns = parserConfig.descriptionColumns ++ [ i ], selected = Nothing } }, Effect.none )

                RemoveDescrColumn i ->
                    ( { model | state = CreateParser file csv { parserConfig | descriptionColumns = parserConfig.descriptionColumns |> List.filter (\j -> j == i), selected = Nothing } }, Effect.none )

                CategoryColumn i ->
                    ( { model | state = CreateParser file csv { parserConfig | categoryColumn = Just i, selected = Nothing } }, Effect.none )

                Name s ->
                    ( { model | state = CreateParser file csv { parserConfig | name = s } }, Effect.none )

                Save importProfile ->
                    let
                        newData =
                            Storage.addImportProfile { importProfile | name = parserConfig.name } data

                        newProfile =
                            Dict.get (newData.autoIncrement - 1) newData.importProfiles |> Maybe.withDefault importProfile
                    in
                    ( { model | state = Parsed file (parseFile file newProfile emptyFilter) }, newData |> Effect.store )

                AbortCustomParser ->
                    ( { model | state = Preview file csv }, Effect.none )

        _ ->
            ( model, Effect.none )


readFile : File -> Cmd Msg
readFile file =
    Task.perform (GotFile <| File.name file) <| File.toString file


parseFileForPreview : Maybe Char -> String -> ParsedRawCsv
parseFileForPreview separator content =
    case separator of
        Nothing ->
            content |> String.trim |> String.split "\n" |> List.map (\row -> [ row ]) |> Ok

        Just c ->
            Parser.parse { fieldSeparator = c } content


parseFile : SelectedFile -> ImportProfile -> ImportFilter -> ParsedFile
parseFile file profile filter =
    ParsedFile profile (CsvParser.parse profile file.content) filter



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    column [ height fill, width fill, spacing size.m ]
        ([ Notification.showNotification model.notification ]
            ++ (case model.state of
                    SelectFile hover ->
                        viewFilePicker hover

                    Preview file csv ->
                        viewCsvPreview Nothing file csv ++ viewProfileSelector data Nothing

                    CreateParser file csv parserConfig ->
                        viewParserBuilder data file csv parserConfig

                    Parsed _ parsed ->
                        viewProfileSelector data (Just parsed.importProfile) ++ viewParsedFile data parsed Nothing
               )
        )



-- FILE PICKER


viewFilePicker : Bool -> List (Element Msg)
viewFilePicker hover =
    [ el
        [ width fill
        , height fill
        , Border.dashed
        , Border.color <|
            if hover then
                color.brightAccent

            else
                color.darkAccent
        , Border.width size.xs
        , Border.rounded size.xl
        , onEvent "dragenter" (D.succeed DragEnter)
        , onEvent "dragover" (D.succeed DragEnter)
        , onEvent "dragleave" (D.succeed DragLeave)
        , onEvent "drop" fileDropDecoder
        ]
        (Input.button ([ centerX, centerY ] ++ style.button) { onPress = Just PickFile, label = text "Select File" })
    ]


fileDropDecoder : D.Decoder Msg
fileDropDecoder =
    D.map GotFileName (D.at [ "dataTransfer", "files" ] (D.oneOrMore (\a -> \_ -> a) File.decoder))


onEvent : String -> D.Decoder msg -> Attribute msg
onEvent event decoder =
    preventDefaultOn event (D.map (\msg -> ( msg, True )) decoder) |> Element.htmlAttribute


storeConfirmation : Int -> Notification Msg
storeConfirmation n =
    "Stored " ++ String.fromInt n ++ " rows in the DB" |> text |> (\t -> Notification.Info [ t ])



-- CSV PREVIEW & PARSER BUILDER


viewCsvPreview : Maybe ParserConfig -> SelectedFile -> ParsedRawCsv -> List (Element Msg)
viewCsvPreview parserConfig file csv =
    case csv of
        Ok rows ->
            [ text <| String.concat [ "Loaded ", file.name, " (", String.fromInt <| List.length rows, " rows). Data preview:" ]
            , previewTable (List.map Array.fromList rows) parserConfig
            ]

        Err problem ->
            case problem of
                SourceEndedWithoutClosingQuote _ ->
                    [ text "Problem parsing CSV file: Unclosed quoted text field." ]

                AdditionalCharactersAfterClosingQuote _ ->
                    [ text "Problem parsing CSV file: Unexpected text after quoted string before field separator." ]


previewTable : List (Array String) -> Maybe ParserConfig -> Element Msg
previewTable csv parserConfig =
    let
        headers =
            csv |> List.head |> Maybe.withDefault Array.empty

        data =
            csv |> List.drop 1 |> List.take 5

        tableCell =
            \i -> Array.get i >> Maybe.withDefault "n/a" >> text

        header =
            parserConfig |> Maybe.map headCell |> Maybe.withDefault (\_ title -> text title)
    in
    indexedTable T.tableStyle
        { data = data
        , columns =
            headers
                |> Array.toList
                |> List.indexedMap
                    (\i title ->
                        T.fullStyledColumn
                            (header i title)
                            (tableCell i)
                    )
        }


headCell : ParserConfig -> Int -> String -> Element Msg
headCell parserConfig i title =
    column [ width fill, spacing size.s ]
        ([ row [ width fill ]
            [ text title
            , plusSquare
                [ alignRight
                , paddingEach { left = size.m, right = 0, top = 0, bottom = 0 }
                , onClick (CustomParser <| Select i)
                ]
                size.m
            ]
         ]
            ++ (if i == (parserConfig.selected |> Maybe.withDefault -1) then
                    if parserConfig.selected == parserConfig.dateColumn then
                        List.map (\( format, label ) -> brightButton (CustomParser (DateFormat format)) label)
                            [ ( YYYYMMDD '-', "Date format: 1970-07-31" )
                            , ( DDMMYYYY '.', "Date format: 31.7.1970" )
                            , ( DDMMYYYY '/', "Date format: 31/7/1970" )
                            ]

                    else
                        [ brightButton (CustomParser (DateColumn i)) "Set as date"
                        , brightButton (CustomParser (AmountColumn i)) "Set as amount"
                        , brightButton (CustomParser (AddDescrColumn i)) "Add to description"
                        , brightButton (CustomParser (RemoveDescrColumn i)) "Remove from description"
                        , brightButton (CustomParser (CategoryColumn i)) "Set as category"
                        ]

                else
                    [ Element.none ]
               )
        )


viewParserBuilder : Data -> SelectedFile -> ParsedRawCsv -> ParserConfig -> List (Element Msg)
viewParserBuilder data file csv parserConfig =
    [ Input.radioRow [ paddingXY size.m 0, spacing size.m ]
        { onChange = \c -> CustomParser (SplitChar c)
        , selected = parserConfig.separator
        , label = Input.labelLeft [] <| text "Column separator: "
        , options =
            [ Input.option ',' (text "Comma")
            , Input.option ';' (text "Semicolon")
            , Input.option '\t' (text "Tab")
            ]
        }
    , text "Selected Columns:"
    , text <| "Date: " ++ (parserConfig.dateColumn |> Maybe.map String.fromInt |> Maybe.withDefault "None")
    , text <| "Amount: " ++ (parserConfig.amountColumn |> Maybe.map String.fromInt |> Maybe.withDefault "None")
    , text <| "Description: " ++ (parserConfig.descriptionColumns |> List.map String.fromInt |> String.join ", ")
    , button (CustomParser AbortCustomParser) "Abort"
    ]
        ++ viewCsvPreview (Just parserConfig) file csv
        ++ (buildParser parserConfig
                |> Maybe.map (\p -> parseFile file p emptyFilter)
                |> Maybe.map (\p -> viewParsedFile data p (Just parserConfig))
                |> Maybe.withDefault []
           )


buildParser : ParserConfig -> Maybe ImportProfile
buildParser parserConfig =
    Maybe.map4
        (\separator date dateFormat amount ->
            importProfile -1 "Preview" separator date parserConfig.descriptionColumns amount dateFormat parserConfig.categoryColumn
        )
        parserConfig.separator
        parserConfig.dateColumn
        parserConfig.dateFormat
        parserConfig.amountColumn



-- VIEW PARSED FILE


viewParsedFile : Data -> ParsedFile -> Maybe ParserConfig -> List (Element Msg)
viewParsedFile data parsedFile parserConfig =
    case parsedFile.parsedRows of
        Err error ->
            [ text "Could not parse the given file with the selected profile.", text (errorToString error) ]

        Ok [] ->
            [ text "The CSV file was empty. There's nothing to do." ]

        Ok rows ->
            let
                annotated =
                    annotate data parsedFile rows
            in
            case parserConfig of
                Just pc ->
                    []
                        ++ [ Input.text [ width <| px 300 ]
                                { onChange = CustomParser << Name
                                , text = pc.name
                                , placeholder = Just <| placeholder [] <| text "Profile Name"
                                , label = labelLeft [ paddingEach { right = size.m, left = 0, top = 0, bottom = 0 } ] <| text "Profile Name"
                                }
                           , Input.button style.button { onPress = Just (CustomParser (Save parsedFile.importProfile)), label = text "Save as New Profile" }
                           ]
                        ++ [ text <| "Preview of the parsed data with the new profile:" ]
                        ++ viewParsedRows annotated.filteredRows

                Nothing ->
                    []
                        ++ viewImportOptions parsedFile.importFilter
                        ++ viewImportWarnings annotated
                        ++ viewImportActions
                            (Dict.values data.accounts)
                            (\a -> Store a parsedFile.importProfile annotated.newCategories (annotated.filteredRows |> List.map .parsedRow))
                        ++ [ text <| "The following " ++ String.fromInt (List.length annotated.filteredRows) ++ " rows shall be added:" ]
                        ++ viewParsedRows annotated.filteredRows


viewImportWarnings : AnnotatedRows -> List (Element msg)
viewImportWarnings ar =
    warning ar.nExcluded (\n -> n ++ " rows are excluded by the date filter.")
        ++ warning (List.length ar.newCategories) (\n -> n ++ " yet unknown categories were found in the CSV. They shall be created upon import: " ++ String.join ", " ar.newCategories)


viewImportActions : List Account -> (Account -> Msg) -> List (Element Msg)
viewImportActions accounts storeMsg =
    [ text <| "Import to account: " ]
        ++ [ row [ spacing size.s ]
                ([ Input.button style.button { onPress = Just Abort, label = text "Abort" } ]
                    ++ (accounts
                            |> List.map (\a -> Input.button style.button { onPress = Just (storeMsg a), label = text a.name })
                       )
                )
           ]


viewParsedRows : List AnnotatedRow -> List (Element Msg)
viewParsedRows annotatedRows =
    case annotatedRows of
        [] ->
            [ text "Every row in this CSV has already been imported. There's nothing to do." ]

        rows ->
            [ indexedTable T.tableStyle
                { data = rows
                , columns =
                    [ T.textColumn "Date" (.parsedRow >> .date >> formatDate)
                    , T.styledColumn "Amount" (.parsedRow >> .amount >> formatEuro)
                    , T.styledColumn "Category" categoryCell
                    , T.textColumn "Description" (.parsedRow >> .description)
                    ]
                }
            ]


viewImportOptions : ImportFilter -> List (Element Msg)
viewImportOptions filter =
    showFilter filter


showFilter : ImportFilter -> List (Element Msg)
showFilter filter =
    [ Input.checkbox []
        { onChange =
            \on ->
                if on then
                    Filter True filter.minDate filter.maxDate

                else
                    Filter False filter.minDate filter.maxDate
        , icon = Input.defaultCheckbox
        , checked = filter.doFilter
        , label = labelRight [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text <| "Filter rows by date before import"
        }
    ]
        ++ (if filter.doFilter then
                [ Input.text [ width <| px 300 ]
                    { onChange = \s -> Filter True s filter.maxDate
                    , text = filter.minDate
                    , placeholder = Just <| placeholder [] <| text "YYYY-MM-DD"
                    , label = labelLeft [ paddingXY size.m 0 ] <| text "min. date"
                    }
                , Input.text [ width <| px 300 ]
                    { onChange = Filter True filter.minDate
                    , text = filter.maxDate
                    , placeholder = Just <| placeholder [] <| text "YYYY-MM-DD"
                    , label = labelLeft [ paddingXY size.m 0 ] <| text "max. date (excl.)"
                    }
                ]

            else
                []
           )



-- VIEW CATEGORIES


categoryCell : AnnotatedRow -> Element msg
categoryCell annotatedRow =
    case annotatedRow.category of
        Nothing ->
            Element.none

        Just None ->
            Element.none

        Just (Known c) ->
            spread (text c.name) (iconTooltip checkMark color.darkAccent "Known Category")

        Just (RuleMatch c) ->
            spread (text c.name) (iconTooltip infoMark color.darkAccent "Matched Categorization Rule")

        Just (Unknown name) ->
            spread (text name) (iconTooltip folderPlus color.darkAccent "New category, will be created upon import.")

        Just (ParsingError invalidShortName) ->
            spread (text invalidShortName) (iconTooltip warnTriangle color.red "Cannot create category, invalid category shortname. Row will be uncategorized.")


spread : Element msg -> Element msg -> Element msg
spread a b =
    row [ width fill ] [ a, el [ width fill ] Element.none, b ]


warning : Int -> (String -> String) -> List (Element msg)
warning number messageTemplate =
    if number == 0 then
        []

    else
        [ row []
            [ warnTriangle [ padding size.xs, Font.color color.red ] size.l
            , number |> String.fromInt |> messageTemplate |> text
            ]
        ]


iconTooltip : (List (Attribute msg) -> Int -> Element msg) -> Color -> String -> Element msg
iconTooltip icon color hint =
    icon [ Font.color color, tooltip onRight hint ] size.m


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



-- CSV PARSING AND ANNOTATIONS


filterRows : ImportFilter -> List ParsedRow -> List ParsedRow
filterRows filter rows =
    if filter.doFilter then
        rows |> List.filter (makeDateFilter filter)

    else
        rows


makeDateFilter : ImportFilter -> (ParsedRow -> Bool)
makeDateFilter filter =
    let
        minFn =
            filter.minDate
                |> toDate (YYYYMMDD '-')
                |> Result.toMaybe
                |> Maybe.map (\min -> \d -> not <| Time.Date.compare min d == GT)
                |> Maybe.withDefault (\_ -> True)

        maxFn =
            filter.maxDate
                |> toDate (YYYYMMDD '-')
                |> Result.toMaybe
                |> Maybe.map (\max -> \d -> Time.Date.compare d max == LT)
                |> Maybe.withDefault (\_ -> True)
    in
    \row -> minFn row.date && maxFn row.date


type alias AnnotatedRow =
    { parsedRow : ParsedRow
    , category : Maybe Categorization
    }


type alias AnnotatedRows =
    { csvSize : Int
    , nExcluded : Int
    , newCategories : List String
    , filteredRows : List AnnotatedRow
    }


annotate : Data -> ParsedFile -> List ParsedRow -> AnnotatedRows
annotate data parsedFile rows =
    let
        csvSize =
            List.length rows

        filteredRows =
            filterRows parsedFile.importFilter rows

        nExcluded =
            List.length rows - List.length filteredRows

        annotatedRows =
            annotateRows
                filteredRows
                (categorize (getCategoryByShort (Dict.values data.categories)) (applyAllCategorizationRules data))

        newCategories =
            annotatedRows
                |> List.Extra.uniqueBy (.parsedRow >> .category)
                |> List.map
                    (.category
                        >> Maybe.andThen
                            (\c ->
                                case c of
                                    Unknown cat ->
                                        Just cat

                                    _ ->
                                        Nothing
                            )
                    )
                |> Maybe.Extra.values
                |> List.sort
    in
    AnnotatedRows csvSize nExcluded newCategories annotatedRows


annotateRows : List ParsedRow -> (ParsedRow -> Maybe Categorization) -> List AnnotatedRow
annotateRows parsedRows categorizeRow =
    parsedRows |> List.map (\row -> AnnotatedRow row (categorizeRow row))


categorize : (String -> Maybe Category) -> (String -> Maybe Category) -> ParsedRow -> Maybe Categorization
categorize categoryLookup categorizationByRules row =
    case row.category of
        Just cat_str ->
            Just (parseCategory categoryLookup cat_str)

        Nothing ->
            categorizationByRules row.description |> matchedCategorization


type Categorization
    = None
    | Known Category
    | RuleMatch Category
    | Unknown String
    | ParsingError String


matchedCategorization : Maybe Category -> Maybe Categorization
matchedCategorization cat =
    Maybe.map (\c -> RuleMatch c) cat


parseCategory : (String -> Maybe Category) -> String -> Categorization
parseCategory categoryLookup categoryName =
    case
        Parser.run CategoryParser.categoryShortNameOrEmpty categoryName
    of
        Ok Nothing ->
            None

        Ok (Just cat) ->
            case categoryLookup cat of
                Just c ->
                    Known c

                Nothing ->
                    Unknown categoryName

        Err _ ->
            ParsingError categoryName


getCategoryForParsedRow : (String -> Maybe Category) -> List Category -> ParsedRow -> Maybe Category
getCategoryForParsedRow categorizationRules categories row =
    case Maybe.andThen (getCategoryByShort categories) row.category of
        Nothing ->
            categorizationRules row.description

        Just c ->
            Just c



-- PROFILE SELECTOR


viewProfileSelector : Data -> Maybe ImportProfile -> List (Element Msg)
viewProfileSelector data profile =
    [ row []
        [ Input.radioRow [ paddingXY size.m 0, spacing size.m ]
            { onChange = ChooseImportProfile
            , selected = profile
            , label = Input.labelLeft [] <| text "Choose import profile: "
            , options =
                Dict.values data.importProfiles
                    |> List.map
                        (\p ->
                            Input.option p (text p.name)
                        )
            }
        , Input.button style.button { onPress = Just (CustomParser StartCustomParser), label = text "Create new Import Profile" }
        ]
    ]
