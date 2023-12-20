module Pages.CategorizationRules exposing (Model, Msg, page)

import Components.Layout as Layout exposing (color, size, style, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Dict
import Element exposing (Element, column, el, indexedTable, padding, paddingXY, row, spacing, text)
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft, placeholder)
import Gen.Params.Accounts exposing (Params)
import Page
import Parser
import Persistence.CategorizationRule exposing (CategorizationRule, categorizationRule)
import Persistence.Data exposing (Data)
import Persistence.Storage as Storage
import Processing.CategoryParser exposing (categoryShortNameOnly)
import Processing.Model exposing (getCategoryByShort)
import Request
import Regex
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = updateOrRedirectOnError shared req update
        , view = viewDataOnly shared view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Editing
    = Off
    | NewCategorizationRule
    | Existing CategorizationRule
    | Deleting CategorizationRule


type alias Model =
    { error : Maybe String
    , editing : Editing
    , pattern : String
    , category : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Off "" "", Cmd.none )


-- UPDATE


type Msg
    = Add
    | EditPattern String
    | EditCategory String
    | Save
    | Abort
    | Delete CategorizationRule
    | DeleteConfirm CategorizationRule
    | EditExisting CategorizationRule


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewCategorizationRule, error = Nothing }, Cmd.none )

        Abort ->
            ( { model | editing = Off, error = Nothing, pattern = "", category = "" }, Cmd.none )

        EditPattern pattern ->
            ( { model | pattern = pattern }, Cmd.none )

        EditCategory category ->
            ( { model | category = String.toUpper category }, Cmd.none )

        EditExisting rule ->
            ( { model | editing = Existing rule, pattern = rule.pattern, category = (Dict.get rule.category data.categories) |> (Maybe.map .short) |> Maybe.withDefault ""}, Cmd.none )

        Save ->
            let
                storeFunction =
                    case model.editing of
                        Existing _ ->
                            Storage.editCategorizationRule

                        _ ->
                            Storage.addCategorizationRule
            in
            case validateCategorizationRule data model of
                Ok a ->
                    ( { model | error = Nothing, pattern = "", category = "", editing = Off }, storeFunction a data |> Storage.store )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )

        Delete rule ->
                ( { model | error = Nothing, editing = Deleting rule }, Cmd.none )

        DeleteConfirm rule ->
            ( { model | error = Nothing, pattern = "", category = "", editing = Off }, Storage.deleteCategorizationRule rule data |> Storage.store )



validateCategorizationRule : Data -> Model -> Result String CategorizationRule
validateCategorizationRule data m =
    let
        id =
            case m.editing of
                Existing rule ->
                    rule.id

                _ ->
                    -1
    in
    Result.map3 categorizationRule
        (Ok id)
        (case Regex.fromString m.pattern of
             Just _ -> Ok m.pattern
             _ -> Err "Pattern is not a valid regular expressions: "
        )
        (if String.isEmpty m.category then
            Err "Short name for category is empty!"
         else
            Parser.run categoryShortNameOnly m.category
                |> Result.mapError (\_ -> "Short name must start with a letter and be uppercase alphanumeric only")
                |> Result.andThen
                    (\validShort ->
                        case getCategoryByShort (Dict.values data.categories) validShort of
                            Nothing -> Err "No category with this short name!"
                            Just c -> Ok c.id
                    )
        )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    Layout.page "Categorization Rules" <|
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
editArea _ model =
    case model.editing of
        Off ->
            button style.button { onPress = Just Add, label = text "Add" }

        Deleting rule ->
            column [ spacing size.m ]
                [ text <| "You are about to delete the rule with pattern '" ++ rule.pattern ++ "'. Continue?"
                , button style.button { onPress = Just (DeleteConfirm rule), label = text "Really Delete" }
                , button style.button { onPress = Just Abort, label = text "Abort" }
                ]

        _ ->
            column [ spacing size.m ]
                [ Input.text []
                    { onChange = EditPattern
                    , text = model.pattern
                    , placeholder = Just <| placeholder [] <| text "Pattern"
                    , label = labelLeft [ paddingXY size.m 0 ] <| text "pattern"
                    }
                , Input.text []
                    { onChange = EditCategory
                    , text = model.category
                    , placeholder = Just <| placeholder [] <| text "Category short name"
                    , label = labelLeft [ paddingXY size.m 0 ] <| text "Category short name (uppercase alphanum)"
                    }
                , button style.button { onPress = Just Save, label = text "Save" }
                , button style.button { onPress = Just Abort, label = text "Abort" }
                ]


showData : Data -> Model -> Element Msg
showData data m =
    if Dict.isEmpty data.categories then
        text "There are no CategorizationRules defined yet"

    else
        indexedTable T.tableStyle
            { data = Dict.values data.categorizationRules
            , columns =
                [ T.styledColumn "Actions"
                    (\a ->
                        row [ spacing size.xs ]
                            [ Input.button style.button { onPress = Just (EditExisting a), label = text "Edit" }
                            , Input.button style.button { onPress = Just (Delete a), label = text "Delete" }
                            ]
                    )
                , T.textColumn "Pattern" .pattern
                , T.textColumn "Category" (\c -> Dict.get c.category data.categories |> Maybe.map .short |> Maybe.withDefault "")
                ]
            }
