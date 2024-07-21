module Pages.Categories exposing (Model, Msg, init, page, update, view)

import Components.Icons exposing (cross)
import Components.Table as T
import Components.Tooltip exposing (tooltip)
import Config exposing (color, paddingBottom, size, style)
import Dict
import Effect exposing (Effect)
import Element exposing (Element, alignRight, below, column, el, fill, indexedTable, padding, px, row, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden, labelLeft, placeholder)
import List.Extra
import Page exposing (Page)
import Parser
import Persistence.Category exposing (Category, CategoryGroup(..), category)
import Persistence.Data exposing (Data)
import Persistence.Storage as Storage
import Processing.CategoryParser exposing (categoryShortNameOnly)
import Processing.Filter exposing (filterCategory)
import Processing.Model exposing (getCategoryByShort, getEntries)
import Processing.Ordering exposing (dateAsc)
import Regex
import Route exposing (Route)
import Shared
import String
import Util.Layout exposing (dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = dataUpdate shared update
        , view = dataView shared "Categories" view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Editing
    = Off
    | NewCategory
    | Existing Category
    | Deleting Category


type alias Model =
    { error : Maybe String
    , editing : Editing
    , name : String
    , short : String
    , group : Maybe CategoryGroup
    , rules : List String
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( Model Nothing Off "" "" Nothing [], Effect.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditShort String
    | EditGroup CategoryGroup
    | EditPattern Int String
    | DeletePattern Int
    | AddPattern String
    | Save
    | Abort
    | Delete Category
    | DeleteConfirm Category
    | EditExisting Category


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewCategory, error = Nothing }, Effect.none )

        Abort ->
            ( { model | editing = Off, error = Nothing, name = "", short = "", group = Nothing }, Effect.none )

        EditName name ->
            ( { model | name = name }, Effect.none )

        EditShort short ->
            ( { model | short = String.toUpper short }, Effect.none )

        EditGroup g ->
            ( { model | group = Just g }, Effect.none )

        EditPattern idx pattern ->
            let
                patterns =
                    model.rules
                        |> List.indexedMap
                            (\index item ->
                                if index == idx then
                                    pattern

                                else
                                    item
                            )
            in
            case Regex.fromString pattern of
                Nothing ->
                    ( { model | error = Just "Invalid regular expression!", rules = patterns }, Effect.none )

                Just _ ->
                    ( { model | error = Nothing, rules = patterns }, Effect.none )

        DeletePattern i ->
            ( { model | rules = List.Extra.removeAt i model.rules }, Effect.none )

        AddPattern p ->
            ( { model | rules = model.rules ++ [ p ] }, Effect.none )

        EditExisting cat ->
            ( { model | error = Nothing, editing = Existing cat, short = cat.short, name = cat.name, group = Just cat.group, rules = cat.rules }, Effect.none )

        Save ->
            let
                storeFunction =
                    case model.editing of
                        Existing _ ->
                            Storage.editCategory

                        _ ->
                            Storage.addCategory
            in
            case validateCategory data model of
                Ok a ->
                    ( { model | error = Nothing, name = "", short = "", rules = [], editing = Off }, storeFunction a data |> Effect.store )

                Err e ->
                    ( { model | error = Just e }, Effect.none )

        Delete cat ->
            let
                n =
                    List.length <| getEntries data [ filterCategory cat ] dateAsc
            in
            if n > 0 then
                ( { model | error = Just ("Cannot delete the category '" ++ cat.name ++ "', it has " ++ String.fromInt n ++ " entries associated.") }, Effect.none )

            else
                ( { model | error = Nothing, editing = Deleting cat }, Effect.none )

        DeleteConfirm cat ->
            ( { model | error = Nothing, name = "", short = "", editing = Off }, Storage.deleteCategory cat data |> Effect.store )



{- This isn't proper parsing at all but it works -}


validateCategory : Data -> Model -> Result String Category
validateCategory data m =
    let
        id =
            case m.editing of
                Existing cat ->
                    cat.id

                _ ->
                    -1
    in
    Result.map5 category
        (Ok id)
        (if String.isEmpty m.name then
            Err "Category name is empty!"

         else
            Ok m.name
        )
        (if String.isEmpty m.short then
            Err "Short name for category is empty!"

         else if String.length (String.trim m.short) > String.length m.name then
            Err "Short name for category is longer than name o.O!"

         else
            Parser.run categoryShortNameOnly m.short
                |> Result.mapError (\_ -> "Short name must start with a letter and be uppercase alphanumeric only")
                |> Result.andThen
                    (\validShort ->
                        case getCategoryByShort (Dict.values data.categories) validShort of
                            Just c ->
                                if c.id /= id then
                                    Err "Short name already used!"

                                else
                                    Ok validShort

                            Nothing ->
                                Ok validShort
                    )
        )
        (m.group |> Result.fromMaybe "A category group needs to be selected.")
        (let
            filtered =
                List.filter (\r -> not (String.isEmpty r)) m.rules
         in
         if List.any invalidRegex filtered then
            Err "Invalid Regular Expression in rules list!"

         else
            Ok filtered
        )


invalidRegex : String -> Bool
invalidRegex s =
    case Regex.fromString s of
        Nothing ->
            True

        Just _ ->
            False



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

        Deleting cat ->
            column [ spacing size.m, width fill ]
                [ text <| "You are about to delete the category '" ++ cat.name ++ "'. Continue?"
                , row [ width fill ]
                    [ button style.button { onPress = Just Abort, label = text "Abort" }
                    , button (style.button ++ [ alignRight ]) { onPress = Just (DeleteConfirm cat), label = text "Really Delete" }
                    ]
                ]

        _ ->
            column [ spacing size.m ]
                ([]
                    ++ [ Input.text [ spacing size.m ]
                            { onChange = EditName
                            , text = model.name
                            , placeholder = Just <| placeholder [] <| text "Category Name"
                            , label = labelLeft [] <| text "Category name"
                            }
                       , Input.text [ spacing size.m ]
                            { onChange = EditShort
                            , text = model.short
                            , placeholder = Just <| placeholder [] <| text "Short input name"
                            , label = labelLeft [] <| text "Short name (uppercase alphanum)"
                            }
                       , text "Matching Patterns:"
                       ]
                    ++ List.indexedMap
                        (\index rule ->
                            row [ width fill, spacing size.s ]
                                [ Input.text []
                                    { onChange = \value -> EditPattern index value
                                    , text = rule
                                    , placeholder = Just <| placeholder [] <| text "Add Regex Pattern"
                                    , label = labelHidden "Pattern"
                                    }
                                , cross [ tooltip below "Delete", onClick (DeletePattern index) ] size.l
                                ]
                        )
                        model.rules
                    ++ [ row [ width fill, spacing size.s ]
                            [ Input.text []
                                { onChange = \value -> AddPattern value
                                , text = ""
                                , placeholder = Just <| placeholder [] <| text "Add Regex Pattern"
                                , label = labelHidden "Pattern"
                                }
                            , el [ width <| px size.l ] Element.none
                            ]
                       , Input.radioRow [ padding size.m, spacing size.m ]
                            { onChange = EditGroup
                            , selected = model.group
                            , label = labelLeft [] (text "Category group")
                            , options =
                                [ Input.option Income (text "Income")
                                , Input.option Expense (text "Expense")
                                , Input.option Internal (text "Internal")
                                ]
                            }
                       , row [ width fill ]
                            [ button style.button { onPress = Just Abort, label = text "Abort" }
                            , button (style.button ++ [ alignRight ]) { onPress = Just Save, label = text "Save" }
                            ]
                       ]
                )


showData : Data -> Model -> Element Msg
showData data _ =
    if Dict.isEmpty data.categories then
        text "There are no categories defined yet"

    else
        indexedTable T.style.table
            { data = Dict.values data.categories
            , columns =
                [ T.styledColumn "Actions"
                    (\a ->
                        row [ spacing size.xs ]
                            [ Input.button style.button { onPress = Just (EditExisting a), label = text "Edit" }
                            , Input.button style.button { onPress = Just (Delete a), label = text "Delete" }
                            ]
                    )
                , T.textColumn "Name" .name
                , T.textColumn "Short" .short
                , T.textColumn "Group" group
                , T.styledColumn "Patterns" showPatterns
                ]
            }


group : Category -> String
group category =
    case category.group of
        Income ->
            "Income"

        Expense ->
            "Expense"

        Internal ->
            "Internal"


showPatterns : Category -> Element Msg
showPatterns category =
    column [ spacing size.s ] (category.rules |> List.map text)
