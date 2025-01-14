module Pages.AddData exposing (Model, Msg, page)

import Components.CsvView as CsvView
import Components.FileDropArea as FileDropArea
import Components.Input exposing (button, disabledButton)
import Components.ProfileBuilder as ProfileBuilder
import Components.RangeSlider as RangeSlider exposing (Selection(..))
import Components.Table as T
import Config exposing (size, style)
import Csv.Decode exposing (Error)
import Csv.Parser exposing (Problem(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Attribute, Color, Element, IndexedColumn, alignRight, alignTop, column, el, fill, height, indexedTable, padding, paddingXY, paragraph, row, spacing, text, width)
import Element.Input as Input
import Layouts
import List.Extra
import Page exposing (Page)
import Pages.Export exposing (beanEntry, codeBlock)
import Persistence.Account exposing (Account)
import Persistence.Category as Category exposing (Category, category)
import Persistence.Data exposing (Data)
import Persistence.ImportProfile exposing (DateFormat(..), ImportProfile)
import Persistence.RawEntry exposing (rawEntry)
import Persistence.Storage as Storage
import Process
import Processing.Annotations exposing (AnnotatedRow, AnnotatedRows, Categorization(..), annotate, categoryCell)
import Processing.BookEntry as BookEntry
import Processing.CategorizationRules exposing (applyAllCategorizationRules)
import Processing.CsvParser as CsvParser exposing (ParsedRow, errorToString)
import Processing.Model exposing (getCategoryForParsedRow)
import Route exposing (Route)
import Route.Path as Path
import Shared exposing (dataSummary)
import Task
import Time.Date as Date exposing (Date)
import Util.Formats as Formats exposing (formatDate, formatEuro)
import Util.Layout exposing (dataInit, dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> dataInit shared initialModel (\_ _ -> initialModel)
        , update = dataUpdate shared update
        , subscriptions = \_ -> Sub.none
        , view = dataView shared "Import Data" view
        }
        |> Page.withLayout (\_ -> Layouts.Tabs { dataSummary = dataSummary shared })



-- INIT


type Model
    = SelectFile (FileDropArea.Model Msg)
    | SetImportOptions LoadedFile
    | CreateProfile (ProfileBuilder.Model Msg)


type alias LoadedFile =
    { name : String
    , content : String
    , parsed : CsvOrData
    , importProfile : Maybe ImportProfile
    , selectedAccount : Maybe Account
    , filter : RangeSlider.Model Date
    , showAsBeancount : Bool
    }


type CsvOrData
    = CSV (Result Problem (List (List String)))
    | ParsedData (Result Error (List ParsedRow))


initialModel : Model
initialModel =
    SelectFile (FileDropArea.init DropAreaMsg LoadFile)



-- UPDATE


type Msg
    = DropAreaMsg FileDropArea.Msg
    | LoadFile String String
    | ChooseImportProfile ImportProfile
    | CreateNewProfile
    | StoreNewProfile ImportProfile
    | ProfileBuilderMsg ProfileBuilder.Msg
    | ChooseAccount Account
    | FilterMsg (RangeSlider.Selection Date)
    | AbortImport
    | ToggleBeancount
    | Store (List ParsedRow) (List String) Account ImportProfile
    | Forward


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case model of
        SelectFile dropArea ->
            updateDropArea data dropArea msg

        SetImportOptions loaded ->
            updateLoaded data loaded msg

        CreateProfile builder ->
            updateCreateProfile data msg builder


updateDropArea : Data -> FileDropArea.Model Msg -> Msg -> ( Model, Effect Msg )
updateDropArea data dropArea msg =
    case msg of
        DropAreaMsg subMsg ->
            let
                ( updated, effect ) =
                    FileDropArea.update subMsg dropArea
            in
            ( SelectFile updated, effect )

        LoadFile name content ->
            let
                loaded =
                    tryProfiles (Dict.values data.importProfiles)
                        { name = name
                        , content = content
                        , parsed = CSV (CsvView.parse Nothing content)
                        , importProfile = Nothing
                        , selectedAccount = Nothing
                        , filter = dateFilter []
                        , showAsBeancount = False
                        }
            in
            ( SetImportOptions loaded, Effect.none )

        Forward ->
            ( initialModel, Effect.pushRoutePath Path.Book )

        _ ->
            ( initialModel, Effect.none )


updateLoaded : Data -> LoadedFile -> Msg -> ( Model, Effect Msg )
updateLoaded data loaded msg =
    case msg of
        ChooseImportProfile profile ->
            ( SetImportOptions (applyProfile profile loaded), Effect.none )

        CreateNewProfile ->
            ( CreateProfile (ProfileBuilder.init loaded.name loaded.content (LoadFile loaded.name loaded.content) StoreNewProfile), Effect.none )

        ChooseAccount acc ->
            ( SetImportOptions { loaded | selectedAccount = Just acc }, Effect.none )

        FilterMsg subMsg ->
            ( SetImportOptions { loaded | filter = RangeSlider.update subMsg loaded.filter }, Effect.none )

        ToggleBeancount ->
            ( SetImportOptions { loaded | showAsBeancount = not loaded.showAsBeancount }, Effect.none )

        Store lines newCats account profile ->
            let
                dataWithNewCategories =
                    newCats
                        |> List.map (\name -> category 0 (name ++ " (auto)") name Category.Expense [])
                        |> (\cs -> Storage.addCategories cs data)

                rowCategory =
                    getCategoryForParsedRow (applyAllCategorizationRules data) (Dict.values dataWithNewCategories.categories)

                rowToRawEntry =
                    \row -> rawEntry account.id profile.id row.rawLine row.date row.amount row.description (rowCategory row)

                newEntries =
                    lines |> List.map rowToRawEntry

                newData =
                    data |> Storage.addEntries newEntries

                min =
                    newEntries |> List.map .date |> List.Extra.minimumWith Date.compare

                max =
                    newEntries |> List.map .date |> List.Extra.maximumWith Date.compare
            in
            -- TODO add a (global) notification about the entries added
            -- model | notification = storeConfirmation (List.length newEntries)
            ( initialModel
            , Effect.batch
                [ Effect.store newData
                , Effect.setDateRange min max
                , Process.sleep 10
                    |> Task.perform (\_ -> Forward)
                    |> Effect.sendCmd
                ]
            )

        _ ->
            ( initialModel, Effect.none )


updateCreateProfile : Data -> Msg -> ProfileBuilder.Model Msg -> ( Model, Effect Msg )
updateCreateProfile data msg builder =
    case msg of
        ProfileBuilderMsg subMsg ->
            let
                ( updated, effect ) =
                    ProfileBuilder.update subMsg builder
            in
            ( CreateProfile updated, effect )

        LoadFile _ _ ->
            update data msg initialModel

        StoreNewProfile p ->
            let
                newData =
                    Storage.addImportProfile p data

                newProfile =
                    Dict.values newData.importProfiles |> List.sortBy .id |> List.reverse |> List.head |> Maybe.withDefault p

                ( newModel, _ ) =
                    update data (LoadFile builder.fileName builder.content) initialModel
            in
            ( newModel, Effect.batch [ Effect.store newData, Effect.sendMsg (ChooseImportProfile newProfile) ] )

        _ ->
            ( initialModel, Effect.none )



-- PROCESSING


applyProfile : ImportProfile -> LoadedFile -> LoadedFile
applyProfile ip file =
    let
        data =
            CsvParser.parse ip file.content
    in
    { file
        | importProfile = Just ip
        , parsed = ParsedData data
        , filter = filterFromData data
    }


tryProfiles : List ImportProfile -> LoadedFile -> LoadedFile
tryProfiles importProfiles loadedFile =
    importProfiles
        |> List.map (\p -> applyProfile p loadedFile)
        |> List.Extra.find isParseSuccess
        |> Maybe.withDefault loadedFile


isParseSuccess : LoadedFile -> Bool
isParseSuccess file =
    case file.parsed of
        ParsedData (Ok _) ->
            True

        _ ->
            False



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    el [ height fill, width fill, spacing size.m, padding size.m ]
        (case model of
            SelectFile area ->
                FileDropArea.view area

            SetImportOptions loaded ->
                viewLoadedFile data loaded

            CreateProfile builder ->
                ProfileBuilder.view builder |> Element.map ProfileBuilderMsg
        )


viewLoadedFile : Data -> LoadedFile -> Element Msg
viewLoadedFile data loaded =
    let
        rows =
            case loaded.parsed of
                ParsedData (Ok r) ->
                    r

                _ ->
                    []

        annotated =
            annotate data loaded.filter rows
    in
    column [ spacing size.m, width fill ]
        [ row [ spacing size.m, width fill ]
            [ filters data loaded
            , actions loaded annotated
            ]
        , dataArea loaded annotated
        ]


filters : Data -> LoadedFile -> Element Msg
filters data loaded =
    column [ spacing size.m ]
        [ text <| "Importing " ++ loaded.name
        , viewProfileSelector data loaded.importProfile
        , viewAccountSelector (Dict.values data.accounts) loaded.selectedAccount
        , RangeSlider.view "Restrict import by date:" Formats.formatDate loaded.filter |> Element.map FilterMsg
        ]


viewProfileSelector : Data -> Maybe ImportProfile -> Element Msg
viewProfileSelector data profile =
    row []
        [ Input.radioRow [ paddingXY size.m 0, spacing size.m ]
            { onChange = ChooseImportProfile
            , selected = profile
            , label = Input.labelLeft [] <| text "Choose import profile: "
            , options = Dict.values data.importProfiles |> List.map (\p -> Input.option p (text p.name))
            }
        , Input.button style.button { onPress = Just CreateNewProfile, label = text "Create new Import Profile" }
        ]


viewAccountSelector : List Account -> Maybe Account -> Element Msg
viewAccountSelector accounts selected =
    Input.radioRow [ paddingXY size.m 0, spacing size.m ]
        { onChange = ChooseAccount
        , selected = selected
        , label = Input.labelLeft [] <| text "Choose account: "
        , options = accounts |> List.map (\p -> Input.option p (text p.name))
        }


actions : LoadedFile -> AnnotatedRows -> Element Msg
actions loaded annotated =
    let
        storeButton =
            (Maybe.map2
                (Store (annotated.filteredRows |> List.map .parsedRow) annotated.newCategories)
                loaded.selectedAccount
                loaded.importProfile
                |> Maybe.map button
                |> Maybe.withDefault disabledButton
            )
                "Import Data"
    in
    column [ alignRight, alignTop, spacing size.m ]
        [ storeButton
        , button AbortImport "Abort"
        , button ToggleBeancount "Toggle Beancount View"
        ]


dataArea : LoadedFile -> AnnotatedRows -> Element Msg
dataArea loaded annotated =
    case loaded.parsed of
        CSV csv ->
            CsvView.view csv

        ParsedData (Ok _) ->
            viewParsedFile annotated loaded.showAsBeancount loaded.selectedAccount

        ParsedData (Err error) ->
            showParseProblem error


showParseProblem : Error -> Element Msg
showParseProblem error =
    column [ spacing size.m ] [ text "Could not parse the given file with the selected profile.", text (errorToString error) ]


toBookEntryCategorization : Categorization -> BookEntry.Categorization
toBookEntryCategorization c =
    case c of
        None ->
            BookEntry.None

        Known category ->
            BookEntry.Single category

        RuleMatch category ->
            BookEntry.Single category

        Unknown string ->
            BookEntry.None

        ParsingError string ->
            BookEntry.None


viewParsedFile : AnnotatedRows -> Bool -> Maybe Account -> Element Msg
viewParsedFile annotated asBeancount account =
    if annotated.filteredRows == [] then
        text "The CSV file was empty. There's nothing to do."

    else if asBeancount then
        let
            acccountName =
                account |> Maybe.map .name |> Maybe.withDefault "Assets:SELECTACCOUNT"

            beCat : AnnotatedRow -> BookEntry.Categorization
            beCat r =
                r.category |> Maybe.withDefault None |> toBookEntryCategorization
        in
        codeBlock
            [ annotated.filteredRows
                |> List.map (\r -> beanEntry r.parsedRow.date r.parsedRow.description acccountName r.parsedRow.amount (beCat r))
            ]

    else
        el [ width fill, height fill ] <|
            indexedTable T.style.fullWidthTable
                { data = annotated.filteredRows
                , columns =
                    [ T.textColumn "Date" (.parsedRow >> .date >> formatDate)
                    , T.styledColumn "Amount" (.parsedRow >> .amount >> formatEuro)
                    , T.styledColumn "Category" categoryCell
                    , T.styledColumn "Description" (.parsedRow >> .description >> text >> (\t -> paragraph [] [ t ]))
                    ]
                }



-- HELPERS


dateFilter : List Date -> RangeSlider.Model Date
dateFilter dates =
    let
        ( head, tail ) =
            dates
                |> List.sortWith Date.compare
                |> List.Extra.unique
                |> List.Extra.uncons
                |> Maybe.withDefault ( Date.date 1970 1 1, [] )
    in
    RangeSlider.init head tail False All


filterFromData : Result Error (List ParsedRow) -> RangeSlider.Model Date
filterFromData data =
    data |> Result.map (List.map .date) |> Result.withDefault [] |> dateFilter
