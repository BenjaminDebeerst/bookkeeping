module Pages.Accounts exposing (Model, Msg, page)

import Components.Layout as Layout exposing (color, formatEuro, style, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Dict
import Element exposing (Element, column, el, indexedTable, text)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder)
import Gen.Params.Accounts exposing (Params)
import Page
import Persistence.Account exposing (Account, AccountStart, account)
import Persistence.Data exposing (Data)
import Persistence.Storage as Storage
import Request
import Shared exposing (Model(..))
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


init : ( Model, Cmd Msg )
init =
    ( Model Nothing False "" "" "" "", Cmd.none )



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditYear String
    | EditMonth String
    | EditBalance String
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

        EditBalance balance ->
            ( { model | balance = balance }, Cmd.none )

        EditYear year ->
            ( { model | year = year }, Cmd.none )

        EditMonth month ->
            ( { model | month = month }, Cmd.none )

        Save ->
            case validateAccount model of
                Ok a ->
                    ( { model | error = Nothing }, Storage.store <| Storage.addAccount a data )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )



{- This isn't proper parsing at all but it works -}


validateAccount : Model -> Result String Account
validateAccount m =
    Result.map4 account
        (Ok m.name)
        (String.toInt m.balance |> Result.fromMaybe "Could not parse the amount")
        (String.toInt m.year |> Result.fromMaybe "Could not parse the year")
        (String.toInt m.month |> Result.fromMaybe "Could not parse the month")



-- VIEW


view : Data -> Model -> View Msg
view data model =
    Layout.page "Accounts" <|
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
