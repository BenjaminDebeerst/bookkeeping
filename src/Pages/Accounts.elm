module Pages.Accounts exposing (Model, Msg, init, initModel, page, update, view)

import Components.Table as T
import Config exposing (color, paddingBottom, size, style)
import Dict
import Effect exposing (Effect)
import Element exposing (Element, column, el, indexedTable, row, shrink, spacing, text)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden, placeholder)
import Page exposing (Page)
import Persistence.Account exposing (Account, AccountStart, account)
import Persistence.Data exposing (Data)
import Persistence.Storage as Storage
import Route exposing (Route)
import Shared
import Time.Date as Date
import Util.Formats exposing (formatEuro, formatYearMonth)
import Util.Layout exposing (dataUpdate, dataView)
import Util.YearMonth as YearMonth


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = dataUpdate shared update
        , view = dataView shared "Accounts" view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Editing
    = Off
    | NewAccount
    | Existing Account
    | Deleting Account


type alias Model =
    { error : Maybe String
    , editing : Editing
    , name : String
    , year : String
    , month : String
    , balance : String
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( initModel, Effect.none )


initModel =
    Model Nothing Off "" "" "" ""



-- UPDATE


type Msg
    = Add
    | EditName String
    | EditYear String
    | EditMonth String
    | EditBalance String
    | Save
    | Abort
    | Delete Account
    | DeleteConfirm Account
    | EditExisting Account


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Add ->
            ( { model | editing = NewAccount, error = Nothing }, Effect.none )

        Abort ->
            init ()

        EditName name ->
            ( { model | name = name }, Effect.none )

        EditBalance balance ->
            ( { model | balance = balance }, Effect.none )

        EditYear year ->
            ( { model | year = year }, Effect.none )

        EditMonth month ->
            ( { model | month = month }, Effect.none )

        EditExisting a ->
            ( { model
                | editing = Existing a
                , name = a.name
                , balance = String.fromInt a.start.amount
                , year = String.fromInt (YearMonth.toDate a.start.yearMonth |> Date.year)
                , month = String.fromInt (YearMonth.toDate a.start.yearMonth |> Date.month)
              }
            , Effect.none
            )

        Save ->
            let
                storeFunction =
                    case model.editing of
                        Existing _ ->
                            Storage.editAccount

                        _ ->
                            Storage.addAccount
            in
            case validateAccount model of
                Ok a ->
                    ( initModel, storeFunction a data |> Effect.store )

                Err e ->
                    ( { model | error = Just e }, Effect.none )

        Delete a ->
            ( { model | error = Nothing, editing = Deleting a }, Effect.none )

        DeleteConfirm a ->
            ( { model | error = Nothing, name = "", editing = Off }, Storage.deleteAccount a data |> Effect.store )



{- This isn't proper parsing at all but it works -}


validateAccount : Model -> Result String Account
validateAccount m =
    let
        id =
            case m.editing of
                Existing a ->
                    a.id

                _ ->
                    -1
    in
    Result.map5 account
        (Ok id)
        (Ok m.name)
        (String.toInt m.balance |> Result.fromMaybe "Could not parse the amount")
        (String.toInt m.year |> Result.fromMaybe "Could not parse the year")
        (String.toInt m.month |> Result.fromMaybe "Could not parse the month")



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

        Deleting a ->
            column [ spacing size.m ]
                [ text <| "You are about to delete the account '" ++ a.name ++ "'. Continue?"
                , button style.button { onPress = Just (DeleteConfirm a), label = text "Really Delete" }
                , button style.button { onPress = Just Abort, label = text "Abort" }
                ]

        _ ->
            column [ spacing size.m ]
                [ Input.text []
                    { onChange = EditName
                    , text = model.name
                    , placeholder = Just <| placeholder [] <| text "Account Name"
                    , label = labelHidden "Account name"
                    }
                , Input.text []
                    { onChange = EditBalance
                    , text = model.balance
                    , placeholder = Just <| placeholder [] <| text "Starting balance (cents)"
                    , label = labelHidden "Account name"
                    }
                , Input.text []
                    { onChange = EditYear
                    , text = model.year
                    , placeholder = Just <| placeholder [] <| text "Year"
                    , label = labelHidden "Account name"
                    }
                , Input.text []
                    { onChange = EditMonth
                    , text = model.month
                    , placeholder = Just <| placeholder [] <| text "Month"
                    , label = labelHidden "Account name"
                    }
                , button style.button { onPress = Just Save, label = text "Save" }
                , button style.button { onPress = Just Abort, label = text "Abort" }
                ]


showData : Data -> Model -> Element Msg
showData data _ =
    if Dict.isEmpty data.accounts then
        text "There are no accounts defined yet"

    else
        indexedTable T.tableStyle
            { data = Dict.values data.accounts
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
                , T.textColumn "Starting Month" (.start >> .yearMonth >> formatYearMonth)
                , T.styledColumn "Starting Balance" (.start >> .amount >> formatEuro)
                ]
            }
