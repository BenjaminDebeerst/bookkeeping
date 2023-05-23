module Pages.Categories exposing (Model, Msg, page)

import Dict
import Element exposing (Element, column, el, indexedTable, paddingXY, row, shrink, spacing, table, text)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden, labelLeft, placeholder)
import Gen.Params.Accounts exposing (Params)
import Layout exposing (color, size, style)
import Page
import Parser exposing (DeadEnd)
import Persistence.Data exposing (Category, Data)
import Persistence.Storage as Storage
import Processing.CategoryParser exposing (categoryShortName)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Editing
    = Off
    | NewCategory
    | Existing Category


type alias Model =
    { error : Maybe String
    , editing : Editing
    , name : String
    , short : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Off "" "", Cmd.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditShort String
    | Save
    | Abort
      --| Delete Int
      --| DeleteConfirm Int
    | EditExisting Category


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewCategory }, Cmd.none )

        Abort ->
            ( { model | editing = Off, error = Nothing, name = "", short = "" }, Cmd.none )

        EditName name ->
            ( { model | name = name }, Cmd.none )

        EditShort short ->
            ( { model | short = short }, Cmd.none )

        EditExisting cat ->
            ( { model | editing = Existing cat, short = cat.short, name = cat.name }, Cmd.none )

        Save ->
            let
                storeFunction =
                    case model.editing of
                        Existing _ ->
                            Storage.editCategory

                        _ ->
                            Storage.addCategory
            in
            case validateCategory model of
                Ok a ->
                    ( { model | error = Nothing, name = "", short = "", editing = Off }, storeFunction a data |> Storage.store )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )



{- This isn't proper parsing at all but it works -}


validateCategory : Model -> Result String Category
validateCategory m =
    let
        id =
            case m.editing of
                Existing cat ->
                    cat.id

                _ ->
                    -1
    in
    Result.map3 makeCategory
        (Ok id)
        (if String.isEmpty m.name then
            Err "Account name is empty!"

         else
            Ok m.name
        )
        (if String.isEmpty m.short then
            Err "Short name for account is empty!"

         else if String.length m.short >= String.length m.name then
            Err "Short name for account is longer than name o.O!"

         else
            Parser.run categoryShortName m.short
                |> Result.mapError (\_ -> "Short name must start with a letter an be alphanumeric only")
        )


makeCategory : Int -> String -> String -> Category
makeCategory id n s =
    Category id n s



-- VIEW


view : Data -> Model -> View Msg
view data model =
    { title = "Categories"
    , body =
        [ Layout.layout "Categories" <|
            column [ spacing size.m ]
                [ errorNotice model.error
                , editArea model.editing model
                , showData data model
                ]
        ]
    }


errorNotice : Maybe String -> Element Msg
errorNotice error =
    case error of
        Nothing ->
            Element.none

        Just message ->
            el [ Font.color color.red ] (text message)


editArea : Editing -> Model -> Element Msg
editArea editing mna =
    if editing == Off then
        button style.button { onPress = Just Add, label = text "Add" }

    else
        column [ spacing size.m ]
            [ Input.text []
                { onChange = EditName
                , text = mna.name
                , placeholder = Just <| placeholder [] <| text "Category Name"
                , label = labelLeft [ paddingXY size.m 0 ] <| text "Category name"
                }
            , Input.text []
                { onChange = EditShort
                , text = mna.short
                , placeholder = Just <| placeholder [] <| text "Short input name"
                , label = labelLeft [ paddingXY size.m 0 ] <| text "Short name"
                }
            , button style.button { onPress = Just Save, label = text "Save" }
            , button style.button { onPress = Just Abort, label = text "Abort" }
            ]


showData : Data -> Model -> Element Msg
showData data _ =
    if Dict.isEmpty data.categories then
        text "There are no categories defined yet"

    else
        indexedTable [ spacing size.tiny ]
            { data = Dict.values data.categories
            , columns =
                [ { header = el style.header <| text "Actions"
                  , width = shrink
                  , view =
                        \i a ->
                            row (style.row i ++ [ spacing size.xs ])
                                [ Input.button style.button { onPress = Just (EditExisting a), label = text "Edit" }
                                , Input.button style.button { onPress = Nothing, label = text "Delete" }
                                ]
                  }
                , { header = el style.header <| text "Name"
                  , width = shrink
                  , view = \i a -> el (style.row i) <| text a.name
                  }
                , { header = el style.header <| text "Short"
                  , width = shrink
                  , view = \i a -> el (style.row i) <| text a.short
                  }
                ]
            }
