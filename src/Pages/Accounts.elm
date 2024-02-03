module Pages.Accounts exposing (Model, Msg, page)

import Components.Table as T
import Config exposing (color, size, style)
import Dict
import Effect exposing (Effect)
import Element exposing (Element, column, el, indexedTable, padding, spacing, text)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder)
import Layouts
import Page exposing (Page)
import Persistence.Account exposing (Account, AccountStart, account)
import Persistence.Data exposing (Data)
import Persistence.Storage as Storage
import Route exposing (Route)
import Shared
import Util.Formats exposing (formatEuro)
import Util.Layout exposing (dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = dataUpdate shared update
        , view = dataView shared "Accounts" view
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Sidebar {})



-- INIT


type alias Model =
    { error : Maybe String
    , editing : Bool
    , name : String
    , year : String
    , month : String
    , balance : String
    }


type alias NewAccount =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( Model Nothing False "" "" "" "", Effect.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditYear String
    | EditMonth String
    | EditBalance String
    | Save
    | Abort


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = True }, Effect.none )

        Abort ->
            ( { model | editing = False }, Effect.none )

        EditName name ->
            ( { model | name = name }, Effect.none )

        EditBalance balance ->
            ( { model | balance = balance }, Effect.none )

        EditYear year ->
            ( { model | year = year }, Effect.none )

        EditMonth month ->
            ( { model | month = month }, Effect.none )

        Save ->
            case validateAccount model of
                Ok a ->
                    ( { model | error = Nothing }, Storage.addAccount a data |> Effect.store )

                Err e ->
                    ( { model | error = Just e }, Effect.none )



{- This isn't proper parsing at all but it works -}


validateAccount : Model -> Result String Account
validateAccount m =
    Result.map4 account
        (Ok m.name)
        (String.toInt m.balance |> Result.fromMaybe "Could not parse the amount")
        (String.toInt m.year |> Result.fromMaybe "Could not parse the year")
        (String.toInt m.month |> Result.fromMaybe "Could not parse the month")



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    column [ padding size.l, spacing size.m ]
        [ errorNotice model.error
        , editArea model.editing model
        , showData data model
        ]


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
                , placeholder = Just <| placeholder [] <| text "Account Name"
                , label = labelHidden "Account name"
                }
            , Element.Input.text []
                { onChange = EditBalance
                , text = mna.balance
                , placeholder = Just <| placeholder [] <| text "Starting balance (cents)"
                , label = labelHidden "Account name"
                }
            , Element.Input.text []
                { onChange = EditYear
                , text = mna.year
                , placeholder = Just <| placeholder [] <| text "Year"
                , label = labelHidden "Account name"
                }
            , Element.Input.text []
                { onChange = EditMonth
                , text = mna.month
                , placeholder = Just <| placeholder [] <| text "Month"
                , label = labelHidden "Account name"
                }
            , button style.button { onPress = Just Save, label = text "Save" }
            , button style.button { onPress = Just Abort, label = text "Abort" }
            ]

    else
        button style.button { onPress = Just Add, label = text "Add" }


showData : Data -> Model -> Element msg
showData data _ =
    if Dict.isEmpty data.accounts then
        text "There are no accounts defined yet"

    else
        indexedTable T.tableStyle
            { data = Dict.values data.accounts
            , columns =
                [ T.textColumn "Id" (.id >> String.fromInt)
                , T.textColumn "Name" .name
                , T.textColumn "Starting Month" (\a -> String.fromInt a.start.year ++ "/" ++ String.fromInt a.start.month)
                , T.styledColumn "Starting Balance" (.start >> .amount >> formatEuro)
                ]
            }
