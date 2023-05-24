module Pages.Accounts exposing (Model, Msg, page)

import Dict
import Element exposing (Element, column, el, shrink, spacing, table, text)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder)
import Gen.Params.Accounts exposing (Params)
import Layout exposing (color, formatEuro, size, style)
import Page
import Persistence.Data exposing (Account, Data)
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
                    ( { model | error = Just ("Storing" ++ Debug.toString a) }, Storage.store <| Storage.addAccount a data )

                Err e ->
                    ( { model | error = Just e }, Cmd.none )



{- This isn't proper parsing at all but it works -}


validateAccount : Model -> Result String Account
validateAccount m =
    Result.map4 makeAccount
        (Ok m.name)
        (String.toInt m.balance |> Result.fromMaybe "Could not parse the amount")
        (String.toInt m.year |> Result.fromMaybe "Could not parse the year")
        (String.toInt m.month |> Result.fromMaybe "Could not parse the month")


makeAccount : String -> Int -> Int -> Int -> Account
makeAccount n a y m =
    Account 0 n (Persistence.Data.AccountStart a y m)



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
        table [ spacing size.s ]
            { data = Dict.values data.accounts
            , columns =
                [ { header = text "Id"
                  , width = shrink
                  , view = \a -> text <| String.fromInt a.id
                  }
                , { header = text "Name"
                  , width = shrink
                  , view = \a -> text <| a.name
                  }
                , { header = text "Starting Month"
                  , width = shrink
                  , view = \a -> text <| String.fromInt a.start.year ++ "/" ++ String.fromInt a.start.month
                  }
                , { header = text "Starting Balance"
                  , width = shrink
                  , view = \a -> formatEuro [] a.start.amount
                  }
                ]
            }
