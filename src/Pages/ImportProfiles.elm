module Pages.ImportProfiles exposing (Model, Msg, init, page, update, view)

import Components.Table as T
import Config exposing (color, paddingBottom, size, style)
import Dict
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, column, el, indexedTable, none, padding, paddingXY, row, shrink, spacing, text)
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft, placeholder)
import Maybe.Extra
import Page exposing (Page)
import Persistence.Data exposing (Data)
import Persistence.ImportProfile exposing (DateFormat(..), ImportProfile, importProfile)
import Persistence.Storage as Storage
import Processing.CsvParser as CsvParser
import Route exposing (Route)
import Shared
import Util.Formats exposing (formatDate, formatEuroStr)
import Util.Layout exposing (dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = dataUpdate shared update
        , view = dataView shared "Import Profiles" view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Editing
    = Off
    | NewImportProfile
    | Existing ImportProfile
    | Deleting ImportProfile


type alias Model =
    { error : Maybe String
    , editing : Editing
    , name : String
    , splitChar : Char
    , dateColumn : String
    , dateFormat : DateFormat
    , descrColumns : String
    , amountColumn : String
    , categoryColumn : String
    , testString : String
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( Model Nothing Off "" ',' "" (YYYYMMDD '-') "" "" "" "", Effect.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditSplitChar Char
    | EditDateColumn String
    | EditDateFormat DateFormat
    | EditAmountColumn String
    | EditDescrColumns String
    | EditCategoryColumn String
    | EditTestString String
    | Save
    | Abort
    | Delete ImportProfile
    | DeleteConfirm ImportProfile
    | EditExisting ImportProfile


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewImportProfile, error = Nothing }, Effect.none )

        Abort ->
            init ()

        EditName name ->
            ( { model | name = name }, Effect.none )

        EditSplitChar char ->
            ( { model | splitChar = char }, Effect.none )

        EditDateColumn string ->
            ( { model | dateColumn = string }, Effect.none )

        EditDateFormat df ->
            ( { model | dateFormat = df }, Effect.none )

        EditDescrColumns string ->
            ( { model | descrColumns = string }, Effect.none )

        EditAmountColumn string ->
            ( { model | amountColumn = string }, Effect.none )

        EditCategoryColumn string ->
            ( { model | categoryColumn = string }, Effect.none )

        EditTestString string ->
            ( { model | testString = string }, Effect.none )

        EditExisting p ->
            ( { model
                | editing = Existing p
                , name = p.name
                , splitChar = p.splitAt
                , dateColumn = String.fromInt p.dateField
                , dateFormat = p.dateFormat
                , descrColumns = p.descrFields |> List.map String.fromInt |> String.join " "
                , amountColumn = String.fromInt p.amountField
                , categoryColumn = p.categoryField |> Maybe.map String.fromInt |> Maybe.withDefault ""
              }
            , Effect.none
            )

        Save ->
            let
                storeFunction =
                    case model.editing of
                        Existing _ ->
                            Storage.editImportProfile

                        _ ->
                            Storage.addImportProfile
            in
            case validateImportProfile data model of
                Ok a ->
                    ( { model | error = Nothing, name = "", editing = Off }, storeFunction a data |> Effect.store )

                Err e ->
                    ( { model | error = Just e }, Effect.none )

        Delete p ->
            ( { model | error = Nothing, editing = Deleting p }, Effect.none )

        DeleteConfirm p ->
            ( { model | error = Nothing, name = "", editing = Off }, Storage.deleteImportProfile p data |> Effect.store )


andMap =
    Result.map2 (|>)


validateImportProfile : Data -> Model -> Result String ImportProfile
validateImportProfile data model =
    let
        id =
            case model.editing of
                Existing p ->
                    p.id

                _ ->
                    -1
    in
    Ok importProfile
        |> andMap (Ok id)
        |> andMap
            (if String.isEmpty model.name then
                Err "Import profile name is empty!"

             else
                Ok model.name
            )
        |> andMap (Ok model.splitChar)
        |> andMap (String.toInt model.dateColumn |> Result.fromMaybe "Date column is not an integer")
        |> andMap (model.descrColumns |> String.split " " |> List.map String.trim |> List.filter (not << String.isEmpty) |> List.map String.toInt |> Maybe.Extra.combine |> Result.fromMaybe "Description columns must be integers")
        |> andMap (String.toInt model.amountColumn |> Result.fromMaybe "Amount column is not an integer")
        |> andMap (Ok model.dateFormat)
        |> andMap
            (if String.isEmpty <| String.trim model.categoryColumn then
                Ok Nothing

             else
                String.toInt model.categoryColumn |> Result.fromMaybe "Category column is not an integer" |> Result.map Just
            )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    column [ spacing size.m, paddingBottom size.m ]
        [ errorNotice model.error
        , editArea data model
        , showData data model
        ]


errorNotice : Maybe String -> Element Msg
errorNotice error =
    case error of
        Nothing ->
            -- The nested none prevents a change in the dom structure when an errorNotice is shown,
            -- which would cause losing the focus on the current input field
            el [] Element.none

        Just message ->
            el [ Font.color color.red ] (text message)


editArea : Data -> Model -> Element Msg
editArea data model =
    case model.editing of
        Off ->
            button style.button { onPress = Just Add, label = text "Add" }

        Deleting p ->
            column [ spacing size.m ]
                [ text <| "You are about to delete the import profile '" ++ p.name ++ "'. Continue?"
                , button style.button { onPress = Just (DeleteConfirm p), label = text "Really Delete" }
                , button style.button { onPress = Just Abort, label = text "Abort" }
                ]

        _ ->
            column [ spacing size.m ] (form data model ++ testArea data model ++ saveAndAbort)


form data model =
    [ el style.h2 (text "Import settings")
    , textInput EditName model.name "Import Profile Name"
    , Input.radioRow [ padding size.m, spacing size.m ]
        { onChange = EditSplitChar
        , selected = Just model.splitChar
        , label = labelLeft [ paddingXY size.m 0 ] (text "CSV split character")
        , options =
            [ Input.option ',' (text "Comma")
            , Input.option ';' (text "Semicolon")
            , Input.option '\t' (text "Tab")
            ]
        }
    , textInput EditDateColumn model.dateColumn "Date Column"
    , Input.radioRow [ padding size.m, spacing size.m ]
        { onChange = EditDateFormat
        , selected = Just model.dateFormat
        , label = labelLeft [ paddingXY size.m 0 ] (text "Date Format")
        , options =
            [ Input.option (YYYYMMDD '-') (text "1970-07-31")
            , Input.option (DDMMYYYY '.') (text "31.7.1970")
            , Input.option (DDMMYYYY '/') (text "31/7/1970")
            ]
        }
    , textInput EditAmountColumn model.amountColumn "Amount Column"
    , textInput EditDescrColumns model.descrColumns "Description Columns"
    , textInput EditCategoryColumn model.categoryColumn "Category Column (optional)"
    ]


testArea data model =
    [ el style.h2 (text "Test the profile settings")
    , textInput EditTestString model.testString "A row from an example CSV"
    , if String.isEmpty model.testString then
        none

      else
        let
            result =
                validateImportProfile data model
                    |> Result.map CsvParser.parseCsvLine
                    |> Result.andThen (\p -> p model.testString)
        in
        case result of
            Ok a ->
                column [ paddingXY size.m 0, spacing size.m ]
                    [ text "Parsing result:"
                    , text <| "Date: " ++ formatDate a.date
                    , text <| "Amount: " ++ formatEuroStr a.amount
                    , text <| "Description: " ++ a.description
                    , text <| "Category: " ++ (a.category |> Maybe.withDefault "none")
                    ]

            Err message ->
                el [ paddingXY size.m 0, Font.color color.red ] (text message)
    ]


saveAndAbort =
    [ button style.button { onPress = Just Save, label = text "Save" }
    , button style.button { onPress = Just Abort, label = text "Abort" }
    ]


textInput : (String -> Msg) -> String -> String -> Element Msg
textInput onChange value label =
    Input.text []
        { onChange = onChange
        , text = value
        , placeholder = Just <| placeholder [] <| text label
        , label = labelLeft [ paddingXY size.m 0 ] <| text label
        }


showData : Data -> Model -> Element Msg
showData data _ =
    if Dict.isEmpty data.importProfiles then
        text "There are no import profiles defined yet"

    else
        indexedTable T.tableStyle
            { data = Dict.values data.importProfiles
            , columns =
                [ { header = el style.header <| text "Actions"
                  , width = shrink
                  , view =
                        \i a ->
                            row (style.row i ++ [ spacing size.xs ])
                                [ Input.button style.button { onPress = Just (EditExisting a), label = text "Edit" }
                                , Input.button style.button { onPress = Just (Delete a), label = text "Delete" }
                                ]
                  }
                , T.textColumn "Name" .name
                , T.textColumn "Split Char" (.splitAt >> String.fromChar)
                , T.textColumn "Date Field" (.dateField >> String.fromInt)
                , T.textColumn "Amount Field" (.amountField >> String.fromInt)
                , T.textColumn "Description Fields" (.descrFields >> List.map String.fromInt >> String.join " ")
                , T.textColumn "Category Field" (.categoryField >> Maybe.map String.fromInt >> Maybe.withDefault "none")
                ]
            }
