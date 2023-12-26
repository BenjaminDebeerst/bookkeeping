module Pages.Categories exposing (Model, Msg, page)

import Components.Layout as Layout exposing (color, size, style, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Dict
import Element exposing (Element, column, el, indexedTable, padding, paddingXY, row, spacing, text)
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft, placeholder)
import Gen.Params.Accounts exposing (Params)
import Page
import Parser
import Persistence.Category exposing (Category, CategoryGroup(..), category)
import Persistence.Data exposing (Data)
import Persistence.Storage as Storage
import Processing.CategoryParser exposing (categoryShortNameOnly)
import Processing.Filter exposing (filterCategory)
import Processing.Model exposing (getCategoryByShort, getEntries)
import Processing.Ordering exposing (dateAsc)
import Regex
import Request
import Shared
import String
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


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Off "" "" Nothing [], Cmd.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditShort String
    | EditGroup CategoryGroup
    | EditPattern Int String
    | AddPatternSlot
    | Save
    | Abort
    | Delete Category
    | DeleteConfirm Category
    | EditExisting Category


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewCategory, error = Nothing }, Cmd.none )

        Abort ->
            ( { model | editing = Off, error = Nothing, name = "", short = "", group = Nothing }, Cmd.none )

        EditName name ->
            ( { model | name = name }, Cmd.none )

        EditShort short ->
            ( { model | short = String.toUpper short }, Cmd.none )

        EditGroup group ->
            ( { model | group = Just group }, Cmd.none )

        EditPattern idx pattern ->
            let patterns = model.rules
                         |> List.indexedMap
                            (\index item -> if index == idx then
                                                pattern
                                            else
                                                item)
            in
                case Regex.fromString pattern of
                    Nothing -> ( { model | error = Just "Invalid regular expression!", rules = patterns }, Cmd.none )
                    Just _ -> ( { model | error = Nothing, rules = patterns }, Cmd.none )

        AddPatternSlot ->
            ( { model | rules = model.rules ++ [ "" ] }, Cmd.none )

        EditExisting cat ->
            ( { model | error = Nothing, editing = Existing cat, short = cat.short, name = cat.name, group = Just cat.group, rules = cat.rules }, Cmd.none )

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
                    ( { model | error = Nothing, name = "", short = "", rules = [], editing = Off }, storeFunction a data |> Storage.store )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )

        Delete cat ->
            let
                n =
                    List.length <| getEntries data [ filterCategory cat ] dateAsc
            in
                if n > 0 then
                    ( { model | error = Just ("Cannot delete the category '" ++ cat.name ++ "', it has " ++ String.fromInt n ++ " entries associated.") }, Cmd.none )
                else
                    ( { model | error = Nothing, editing = Deleting cat }, Cmd.none )

        DeleteConfirm cat ->
            ( { model | error = Nothing, name = "", short = "", editing = Off }, Storage.deleteCategory cat data |> Storage.store )



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
        (
         let
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
        Nothing -> True
        Just _  -> False

-- VIEW

view : Data -> Model -> View Msg
view data model =
    Layout.page "Categories" <|
        [ errorNotice model.error
        , editArea data model
        , showData data model
        ]


errorNotice : Maybe String -> Element Msg
errorNotice error =
    case error of
        Nothing ->
            el [] Element.none -- this prevents a change in the dom structure when an errorNotice is shown, which would cause losing the focus on the current input field

        Just message ->
            el [ Font.color color.red ] (text message)


editArea : Data -> Model -> Element Msg
editArea data model =
    case model.editing of
        Off ->
            button style.button { onPress = Just Add, label = text "Add" }

        Deleting cat ->
            column [ spacing size.m ]
                [ text <| "You are about to delete the category '" ++ cat.name ++ "'. Continue?"
                , button style.button { onPress = Just (DeleteConfirm cat), label = text "Really Delete" }
                , button style.button { onPress = Just Abort, label = text "Abort" }
                ]

        _ ->
            column [ spacing size.m ]
                ([]
                 ++ [ Input.text []
                         { onChange = EditName
                         , text = model.name
                         , placeholder = Just <| placeholder [] <| text "Category Name"
                         , label = labelLeft [ paddingXY size.m 0 ] <| text "Category name"
                         }
                   , Input.text []
                       { onChange = EditShort
                       , text = model.short
                       , placeholder = Just <| placeholder [] <| text "Short input name"
                       , label = labelLeft [ paddingXY size.m 0 ] <| text "Short name (uppercase alphanum)"
                       }
                   ]
                 ++ List.indexedMap (\index rule -> Input.text[] { onChange = (\value -> EditPattern index value)
                                                                 , text = rule
                                                                 , placeholder = Just <| placeholder [] <| text "Regex Pattern"
                                                                 , label = labelLeft [ paddingXY size.m 0 ] <| text "Pattern"
                                                                 }
                                    ) model.rules
                 ++ [ (row [] [ button style.button { onPress = Just AddPatternSlot, label = text "Add Pattern" }
                             , text " (leave patterns empty to delete)" ])
                    , Input.radioRow [ padding size.m, spacing size.m ]
                        { onChange = EditGroup
                        , selected = model.group
                        , label = labelLeft [ paddingXY size.m 0 ] (text "Category group")
                        , options =
                              [ Input.option Income (text "Income")
                              , Input.option Expense (text "Expense")
                              , Input.option Internal (text "Internal")
                              ]
                        }
                    , button style.button { onPress = Just Save, label = text "Save" }
                    , button style.button { onPress = Just Abort, label = text "Abort" }
                    ])



showData : Data -> Model -> Element Msg
showData data _ =
    if Dict.isEmpty data.categories then
        text "There are no categories defined yet"

    else
        indexedTable T.tableStyle
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
                , T.textColumn "Group" (.group >> Debug.toString)
                ]
            }
