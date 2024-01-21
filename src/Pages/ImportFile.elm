module Pages.ImportFile exposing (Model, Msg, page)

import Components.Icons exposing (checkMark, copy, folderPlus, infoMark, warnTriangle)
import Components.Layout as Layout exposing (color, formatDate, formatEuro, size, style, tooltip, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Csv.Decode as Decode exposing (Error(..))
import Csv.Parser as Parser exposing (Problem(..))
import Dict exposing (Dict)
import Element exposing (Attribute, Color, Element, IndexedColumn, centerX, centerY, el, fill, height, indexedTable, onRight, padding, paddingEach, paddingXY, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelLeft, labelRight, placeholder)
import File exposing (File)
import File.Select as Select
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Page exposing (Page)
import Parser
import Persistence.Account exposing (Account)
import Persistence.Category as Category exposing (Category, category)
import Persistence.Data exposing (Data)
import Persistence.ImportProfile exposing (DateFormat(..), ImportProfile)
import Persistence.RawEntry exposing (rawEntry, sha1)
import Persistence.Storage as Storage
import Processing.CategorizationRules exposing (applyAllCategorizationRules)
import Processing.CategoryParser as CategoryParser
import Processing.CsvParser as CsvParser exposing (ParsedRow, toDate)
import Processing.Model exposing (getCategoryByShort)
import Route exposing (Route)
import Shared exposing (Model(..))
import Task
import Time.Date
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
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
    | Show RawCsv (Maybe ParsedFile)
    | Stored Int


type alias RawCsv =
    { name : String
    , content : String
    , parsed : Result Problem (List (List String))
    }


type alias ParsedFile =
    { importProfile : ImportProfile
    , parsedRows : Result Error (List ParsedRow)
    , importFilter : ImportFilter
    , generateIds : Bool
    }


type alias ImportFilter =
    { doFilter : Bool
    , minDate : String
    , maxDate : String
    }


emptyFilter =
    ImportFilter False "" ""


show : String -> String -> Model
show file content =
    Show (RawCsv file content (Parser.parse { fieldSeparator = ',' } content)) Nothing



-- UPDATE


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File
    | GotFile String String
    | ChooseImportProfile ImportProfile
    | Filter Bool String String
    | GenerateIds Bool
    | Abort
    | Store Account ImportProfile (List String) (List ParsedRow) Bool


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
            ( show name content, Cmd.none )

        ChooseImportProfile profile ->
            case model of
                Show csv parsed ->
                    ( Show csv
                        (Just
                            (ParsedFile
                                profile
                                (CsvParser.parse profile csv.content)
                                (parsed |> Maybe.map .importFilter |> Maybe.withDefault emptyFilter)
                                False
                            )
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Filter on start end ->
            case model of
                Show csv (Just parsed) ->
                    ( Show csv (Just { parsed | importFilter = ImportFilter on start end }), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GenerateIds on ->
            case model of
                Show csv (Just parsed) ->
                    ( Show csv (Just { parsed | generateIds = on }), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Abort ->
            ( Pick, Cmd.none )

        Store account profile newCats lines generateIds ->
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
                    dataWithNewCategories |> Storage.addEntries generateIds newEntries
            in
            ( Stored (List.length newEntries)
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
            Show csv parsed ->
                viewFileContents data csv parsed

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


viewFileContents : Data -> RawCsv -> Maybe ParsedFile -> List (Element Msg)
viewFileContents data csv parsed =
    preview csv
        ++ (csv.parsed
                |> Result.map (\_ -> viewProfileSelector data (parsed |> Maybe.map .importProfile))
                |> Result.withDefault []
           )
        ++ (parsed
                |> Maybe.map (\file -> showFile data file)
                |> Maybe.withDefault []
           )


showFile : Data -> ParsedFile -> List (Element Msg)
showFile data parsedFile =
    case parsedFile.parsedRows of
        Err error ->
            [ text "Could not parse the given file with the selected profile.", text (errorToString error) ]

        Ok [] ->
            [ text "The CSV file was empty. There's nothing to do." ]

        Ok rows ->
            viewParsedRows
                (Dict.values data.accounts)
                (\s -> Dict.member s data.rawEntries)
                (getCategoryByShort (Dict.values data.categories))
                (applyAllCategorizationRules data)
                parsedFile.importProfile
                parsedFile.importFilter
                parsedFile.generateIds
                rows


viewParsedRows : List Account -> (String -> Bool) -> (String -> Maybe Category) -> (String -> Maybe Category) -> ImportProfile -> ImportFilter -> Bool -> List ParsedRow -> List (Element Msg)
viewParsedRows accounts entryIdExists findCategory categorizationByRules profile filter generateIds rows =
    let
        csvSize =
            List.length rows |> String.fromInt

        filteredRows =
            filterRows filter rows

        nExcluded =
            List.length rows - List.length filteredRows

        duplicates =
            if generateIds then
                Dict.empty

            else
                findDuplicateRows filteredRows

        ( annotatedRows, overlap ) =
            annotateRows
                filteredRows
                duplicates
                (if generateIds then
                    \_ -> False

                 else
                    entryIdExists
                )
                findCategory
                categorizationByRules

        annotatedRowsToStore =
            annotatedRows
                |> List.filter (\ar -> not ar.alreadyImported)
                |> (if generateIds then
                        -- simply skip don't do the uniqueness filter
                        identity

                    else
                        List.Extra.uniqueBy (\ar -> sha1 ar.parsedRow.rawLine)
                   )

        rowsToStore =
            annotatedRowsToStore |> List.map .parsedRow

        storeSize =
            List.length rowsToStore

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
    if storeSize == 0 then
        showImportOptions filter generateIds
            ++ [ text "Every row in this CSV has already been imported. There's nothing to do." ]

    else
        showImportOptions filter generateIds
            ++ warning nExcluded (\n -> n ++ " rows are excluded by the date filter.")
            ++ warning overlap (\n -> n ++ " of " ++ csvSize ++ " rows in this CSV have already been imported. They shall be skipped.")
            ++ warning (Dict.size duplicates) (\_ -> "This CSV has " ++ csvSize ++ " rows, but only " ++ String.fromInt storeSize ++ " distinct duplicated rows. Duplicate rows will be imported only once. Are your rows sufficiently distinct?")
            ++ warning (List.length newCategories) (\n -> n ++ " yet unknown categories were found in the CSV. They shall be created upon import: " ++ String.join ", " newCategories)
            ++ [ text <| "Import to account: " ]
            ++ [ row [ spacing size.s ]
                    ([ Input.button style.button { onPress = Just Abort, label = text "Abort" } ]
                        ++ (accounts
                                |> List.map (\a -> Input.button style.button { onPress = Just (Store a profile newCategories rowsToStore generateIds), label = text a.name })
                           )
                    )
               ]
            ++ [ text <| "The following " ++ String.fromInt storeSize ++ " rows shall be added:"
               , indexedTable T.tableStyle
                    { data = annotatedRowsToStore
                    , columns =
                        [ T.styledColumn "Info" <| annotation
                        , T.textColumn "Date" (.parsedRow >> .date >> formatDate)
                        , T.styledColumn "Amount" (.parsedRow >> .amount >> formatEuro)
                        , T.styledColumn "Category" categoryCell
                        , T.textColumn "Description" (.parsedRow >> .description)
                        ]
                    }
               ]


showImportOptions : ImportFilter -> Bool -> List (Element Msg)
showImportOptions filter generateIds =
    showGenerateIdsOption generateIds ++ showFilter filter


showGenerateIdsOption : Bool -> List (Element Msg)
showGenerateIdsOption generateIds =
    [ Input.checkbox []
        { onChange =
            \on ->
                if on then
                    GenerateIds True

                else
                    GenerateIds False
        , icon = Input.defaultCheckbox
        , checked = generateIds
        , label = labelRight [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text <| "Generate ids for rows. Do not assume content-based identity."
        }
    ]


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


annotation : AnnotatedRow -> Element msg
annotation annotatedRow =
    if annotatedRow.duplicates > 1 then
        iconTooltip copy color.black <|
            "This row was found "
                ++ String.fromInt annotatedRow.duplicates
                ++ " times."

    else
        Element.none


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



-- CSV parsing and annotations


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
    , duplicates : Int
    , alreadyImported : Bool
    , category : Maybe Categorization
    }


annotateRows : List ParsedRow -> Dict String Int -> (String -> Bool) -> (String -> Maybe Category) -> (String -> Maybe Category) -> ( List AnnotatedRow, Int )
annotateRows parsedRows duplicates entryIdExists categoryLookup categorizationByRules =
    parsedRows
        |> List.foldr
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
                        case row.category of
                            Just cat_str ->
                                Just (parseCategory categoryLookup cat_str)

                            Nothing ->
                                categorizationByRules row.description |> matchedCategorization
                in
                ( AnnotatedRow row dups exists cat :: rows, m )
            )
            ( [], 0 )


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


getCategoryForParsedRow : (String -> Maybe Category) -> List Category -> ParsedRow -> Maybe Category
getCategoryForParsedRow categorizationRules categories row =
    case Maybe.andThen (getCategoryByShort categories) row.category of
        Nothing ->
            categorizationRules row.description

        Just c ->
            Just c



-- raw CSV (pre)view


preview : RawCsv -> List (Element Msg)
preview csv =
    case csv.parsed of
        Ok rows ->
            [ text <| "Loaded header and " ++ (rows |> List.length |> (\i -> i - 1) |> String.fromInt) ++ " rows from " ++ csv.name ++ ". This is how the raw data looks like:"
            , previewTable rows
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



-- profile selector


viewProfileSelector : Data -> Maybe ImportProfile -> List (Element Msg)
viewProfileSelector data profile =
    [ Input.radioRow []
        { onChange = ChooseImportProfile
        , selected = profile
        , label = Input.labelLeft [ paddingXY size.m 0 ] <| text "Choose import profile: "
        , options =
            Dict.values data.importProfiles
                |> List.map
                    (\p ->
                        Input.option p (el [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text p.name)
                    )
        }
    ]
