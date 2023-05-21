module Pages.Categories exposing (Model, Msg, page)

import Dict
import Element exposing (Element, column, el, shrink, spacing, table, text)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder)
import Gen.Params.Accounts exposing (Params)
import Layout exposing (color, size, style)
import Page
import Persistence.Data exposing (Category, Data)
import Persistence.Storage as Storage
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


type alias Model =
    { error : Maybe String
    , editing : Bool
    , name : String
    , short : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing False "" "", Cmd.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditShort String
    | Save
    | Abort


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = True }, Cmd.none )

        Abort ->
            ( { model | editing = False }, Cmd.none )

        EditName name ->
            ( { model | name = name }, Cmd.none )

        EditShort short ->
            ( { model | short = short }, Cmd.none )

        Save ->
            case validateCategory model of
                Ok a ->
                    ( { model | error = Just ("Storing" ++ Debug.toString a) }, Storage.addCategory a data |> Storage.store )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )



{- This isn't proper parsing at all but it works -}


validateCategory : Model -> Result String Category
validateCategory m =
    Result.map2 makeCategory
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
            Ok m.short
        )


makeCategory : String -> String -> Category
makeCategory n s =
    Category 0 n s



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


editArea : Bool -> Model -> Element Msg
editArea editing mna =
    if editing then
        column []
            [ Element.Input.text []
                { onChange = EditName
                , text = mna.name
                , placeholder = Just <| placeholder [] <| text "Category Name"
                , label = labelHidden "Category name"
                }
            , Element.Input.text []
                { onChange = EditShort
                , text = mna.short
                , placeholder = Just <| placeholder [] <| text "Short input name"
                , label = labelHidden "Short name"
                }
            , button style.button { onPress = Just Save, label = text "Save" }
            , button style.button { onPress = Just Abort, label = text "Abort" }
            ]

    else
        button style.button { onPress = Just Add, label = text "Add" }


showData : Data -> Model -> Element msg
showData data _ =
    if Dict.isEmpty data.categories then
        text "There are no categories defined yet"

    else
        table [ spacing size.s ]
            { data = Dict.values data.categories
            , columns =
                [ { header = text "Id"
                  , width = shrink
                  , view = \a -> text <| String.fromInt a.id
                  }
                , { header = text "Name"
                  , width = shrink
                  , view = \a -> text <| a.name
                  }
                , { header = text "Short"
                  , width = shrink
                  , view = \a -> text <| a.short
                  }
                ]
            }
