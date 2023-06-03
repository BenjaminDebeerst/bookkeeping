module Pages.ImportProfiles exposing (Model, Msg, page)

import Dict
import Dropdown
import Element exposing (Element, IndexedColumn, column, el, indexedTable, none, padding, paddingXY, row, shrink, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft, placeholder)
import Gen.Params.Accounts exposing (Params)
import Layout exposing (color, size, style)
import Maybe.Extra
import Page
import Persistence.Data exposing (Account, Category, Data, DateFormat(..), ImportProfile)
import Persistence.Storage as Storage
import Processing.CsvParser as CsvParser
import Request
import View exposing (View)


page : Data -> Request.With Params -> Page.With Model Msg
page data req =
    Page.element
        { init = init
        , update = update data
        , view = view data
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Editing
    = Off
    | NewCategory
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
    , account : Maybe Account
    , accountDropdownState : Dropdown.State Account
    , testString : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Off "" ',' "" (YYYYMMDD '-') "" "" "" Nothing (Dropdown.init "") "", Cmd.none )



-- UPDATE
--{ id : Int
--, name : String
--, splitAt : Char
--, dateField : Int
--, descrFields : List Int
--, amountField : Int
--, dateFormat : String
--, categoryField : Maybe Int
--}


type Msg
    = Add
    | EditName String
    | EditAccount (Maybe Account)
    | AccountDropdownMsg (Dropdown.Msg Account)
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


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewCategory, error = Nothing }, Cmd.none )

        Abort ->
            init

        EditName name ->
            ( { model | name = name }, Cmd.none )

        EditSplitChar char ->
            ( { model | splitChar = char }, Cmd.none )

        EditDateColumn string ->
            ( { model | dateColumn = string }, Cmd.none )

        EditDateFormat df ->
            ( { model | dateFormat = df }, Cmd.none )

        EditDescrColumns string ->
            ( { model | descrColumns = string }, Cmd.none )

        EditAmountColumn string ->
            ( { model | amountColumn = string }, Cmd.none )

        EditCategoryColumn string ->
            ( { model | categoryColumn = string }, Cmd.none )

        EditAccount option ->
            ( { model | account = option }, Cmd.none )

        EditTestString string ->
            ( { model | testString = string }, Cmd.none )

        AccountDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (dropdownConfig data) subMsg model model.accountDropdownState
            in
            ( { model | accountDropdownState = state }, cmd )

        EditExisting p ->
            ( { model
                | editing = Existing p
                , name = p.name
                , account = Dict.get p.accountId data.accounts
                , splitChar = p.splitAt
                , dateColumn = String.fromInt p.dateField
                , dateFormat = p.dateFormat
                , descrColumns = p.descrFields |> List.map String.fromInt |> String.join " "
                , amountColumn = String.fromInt p.amountField
              }
            , Cmd.none
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
                    ( { model | error = Nothing, name = "", editing = Off }, storeFunction a data |> Storage.store )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )

        Delete p ->
            ( { model | error = Nothing, editing = Deleting p }, Cmd.none )

        DeleteConfirm p ->
            ( { model | error = Nothing, name = "", editing = Off }, Storage.deleteImportProfile p data |> Storage.store )


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
    Ok ImportProfile
        |> andMap (Ok id)
        |> andMap
            (if String.isEmpty model.name then
                Err "Import profile name is empty!"

             else
                Ok model.name
            )
        |> andMap (model.account |> Maybe.map .id |> Result.fromMaybe "No account selected")
        |> andMap (Ok model.splitChar)
        |> andMap (String.toInt model.dateColumn |> Result.fromMaybe "Date column is not an integer")
        |> andMap (model.descrColumns |> String.split " " |> List.map String.trim |> List.filter (not << String.isEmpty) |> List.map String.toInt |> Maybe.Extra.combine |> Result.fromMaybe "Description columns must be integers")
        |> andMap (String.toInt model.amountColumn |> Result.fromMaybe "Amount column is not an integer")
        -- TODO implement date formats
        |> andMap (Ok model.dateFormat)
        -- TODO implement optional category parsing
        |> andMap (Ok Nothing)


dropdownConfig : Data -> Dropdown.Config Account Msg Model
dropdownConfig data =
    let
        itemToPrompt item =
            text item.name

        itemToElement selected highlighted item =
            el
                [ padding size.s
                , Background.color
                    (if highlighted then
                        color.extraBrightAccent

                     else
                        color.white
                    )
                ]
                (text item.name)

        accounts =
            Dict.values data.accounts
    in
    Dropdown.withListAttributes
        [ Background.color color.white
        , Border.color color.darkAccent
        , Border.shadow
            { offset = ( 0, 1 )
            , size = 0.01
            , blur = 5
            , color = color.black
            }
        ]
    <|
        Dropdown.basic
            { itemsFromModel = always accounts
            , selectionFromModel = \m -> m.account
            , dropdownMsg = AccountDropdownMsg
            , onSelectMsg = EditAccount
            , itemToPrompt = itemToPrompt
            , itemToElement = itemToElement
            }



-- VIEW


view : Data -> Model -> View Msg
view data model =
    Layout.page "Import Profiles"
        [ errorNotice model.error
        , editArea data model
        , showData data model
        ]


errorNotice : Maybe String -> Element Msg
errorNotice error =
    case error of
        Nothing ->
            Element.none

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
    , row [ paddingXY size.m 0, spacing size.m ]
        [ text "Choose Account to import for: "
        , Dropdown.view (dropdownConfig data) model model.accountDropdownState
        ]
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
                text (Debug.toString a)

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
        indexedTable [ spacing size.tiny ]
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
                , tableColumn "Name" .name
                , tableColumn "Account" (.accountId >> (\i -> Dict.get i data.accounts) >> Maybe.map .name >> Maybe.withDefault "Not found")
                , tableColumn "Split Char" (.splitAt >> String.fromChar)
                , tableColumn "Date Field" (.dateField >> String.fromInt)
                , tableColumn "Amount Field" (.amountField >> String.fromInt)
                , tableColumn "Description Fields" (.descrFields >> List.map String.fromInt >> String.join " ")
                ]
            }


tableColumn : String -> (record -> String) -> IndexedColumn record msg
tableColumn title textFromRow =
    { header = el style.header <| text title
    , width = shrink
    , view = \i e -> el (style.row i) <| text <| textFromRow e
    }
