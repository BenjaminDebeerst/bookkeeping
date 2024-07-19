module Components.ProfileBuilder exposing (..)

import Components.CsvView as CsvView
import Components.Icons exposing (plusSquare)
import Components.Input exposing (brightButton, button, disabledButton)
import Config exposing (size)
import Csv.Parser as Parser exposing (Problem)
import Effect exposing (Effect)
import Element exposing (Element, alignRight, column, fill, paddingEach, paddingXY, row, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Input as Input exposing (labelLeft, placeholder)
import Persistence.ImportProfile as ImportProfile exposing (AmountField, DateFormat(..), ImportProfile, importProfile)


type Msg
    = SplitChar Char
    | Select Int
    | DateColumn Int
    | DateFormat DateFormat
    | SimpleAmount Int
    | CreditAmount Int
    | DebitAmount Int
    | AddDescrColumn Int
    | RemoveDescrColumn Int
    | CategoryColumn Int
    | Name String
    | Abort
    | Store ImportProfile


type alias Model msg =
    { abort : msg
    , store : ImportProfile -> msg
    , fileName : String
    , content : String
    , csv : Result Problem (List (List String))
    , profileName : String
    , separator : Char
    , dateColumn : Maybe Int
    , dateFormat : Maybe DateFormat
    , amountColumn : AmountColumn
    , descriptionColumns : List Int
    , categoryColumn : Maybe Int
    , selected : Maybe Int
    }


type AmountColumn
    = NoneSelected
    | Simple Int
    | CreditDebit (Maybe Int) (Maybe Int)


init : String -> String -> msg -> (ImportProfile -> msg) -> Model msg
init fileName content abort store =
    Model
        abort
        store
        fileName
        content
        (Parser.parse { fieldSeparator = ',' } content)
        ""
        ','
        Nothing
        Nothing
        NoneSelected
        []
        Nothing
        Nothing


update : Msg -> Model msg -> ( Model msg, Effect msg )
update msg model =
    case msg of
        SplitChar char ->
            ( { model | separator = char, selected = Nothing, csv = Parser.parse { fieldSeparator = char } model.content }, Effect.none )

        Select i ->
            ( { model | selected = Just i }, Effect.none )

        DateColumn i ->
            ( { model | dateColumn = Just i }, Effect.none )

        DateFormat format ->
            ( { model | dateFormat = Just format, selected = Nothing }, Effect.none )

        SimpleAmount i ->
            ( { model | amountColumn = Simple i, selected = Nothing }, Effect.none )

        CreditAmount i ->
            case model.amountColumn of
                CreditDebit _ d ->
                    ( { model | amountColumn = CreditDebit (Just i) d, selected = Nothing }, Effect.none )

                _ ->
                    ( { model | amountColumn = CreditDebit (Just i) Nothing, selected = Nothing }, Effect.none )

        DebitAmount i ->
            case model.amountColumn of
                CreditDebit c _ ->
                    ( { model | amountColumn = CreditDebit c (Just i), selected = Nothing }, Effect.none )

                _ ->
                    ( { model | amountColumn = CreditDebit Nothing (Just i), selected = Nothing }, Effect.none )

        AddDescrColumn i ->
            ( { model | descriptionColumns = model.descriptionColumns ++ [ i ], selected = Nothing }, Effect.none )

        RemoveDescrColumn i ->
            ( { model | descriptionColumns = model.descriptionColumns |> List.filter (\j -> j == i), selected = Nothing }, Effect.none )

        CategoryColumn i ->
            ( { model | categoryColumn = Just i, selected = Nothing }, Effect.none )

        Name s ->
            ( { model | profileName = s }, Effect.none )

        Abort ->
            ( model, Effect.sendMsg model.abort )

        Store ip ->
            ( model, Effect.sendMsg (model.store ip) )


view : Model msg -> Element Msg
view model =
    -- TODO consider showing a parsed preview after the CSV view
    -- in case a complete import profile is available
    column [ spacing size.m, width fill ]
        [ viewActions model
        , CsvView.viewWithCustomHeaders model.csv (editHeaders model)
        ]


viewActions : Model msg -> Element Msg
viewActions model =
    column [ spacing size.m ] <|
        [ Input.radioRow [ paddingXY size.m 0, spacing size.m ]
            { onChange = \c -> SplitChar c
            , selected = Just model.separator
            , label = Input.labelLeft [] <| text "Column separator: "
            , options =
                [ Input.option ',' (text "Comma")
                , Input.option ';' (text "Semicolon")
                , Input.option '\t' (text "Tab")
                ]
            }
        , text "Selected Columns:"
        , text <| "Date: " ++ (model.dateColumn |> Maybe.map String.fromInt |> Maybe.withDefault "None")
        , text <| "Amount: " ++ amountColumnString model.amountColumn
        , text <| "Description: " ++ (model.descriptionColumns |> List.map String.fromInt |> String.join ", ")
        , Input.text [ spacing size.m ]
            { onChange = Name
            , text = model.profileName
            , placeholder = Just <| placeholder [] <| text "Profile name"
            , label = labelLeft [] <| text "Profile name:"
            }
        , button Abort "Abort"
        , buildProfile model |> Maybe.map (\p -> button (Store p) "Save Import Profile") |> Maybe.withDefault (disabledButton "Save Import Profile")
        ]


editHeaders : Model msg -> CsvView.Headers Msg
editHeaders model =
    \( i, title ) -> Just (headCell model i title)


headCell : Model msg -> Int -> String -> Element Msg
headCell model i title =
    column [ width fill, spacing size.s ]
        ([ row [ width fill ]
            [ text title
            , plusSquare
                [ alignRight
                , paddingEach { left = size.m, right = 0, top = 0, bottom = 0 }
                , onClick (Select i)
                ]
                size.m
            ]
         ]
            ++ (if i == (model.selected |> Maybe.withDefault -1) then
                    if model.selected == model.dateColumn then
                        List.map (\( format, label ) -> brightButton (DateFormat format) label)
                            [ ( YYYYMMDD '-', "Date format: 1970-07-31" )
                            , ( DDMMYYYY '.', "Date format: 31.7.1970" )
                            , ( DDMMYYYY '/', "Date format: 31/7/1970" )
                            ]

                    else
                        [ brightButton (DateColumn i) "Set as date"
                        , brightButton (SimpleAmount i) "Set as amount"
                        , brightButton (CreditAmount i) "Set as credit amount"
                        , brightButton (DebitAmount i) "Set as debit amount"
                        , brightButton (AddDescrColumn i) "Add to description"
                        , brightButton (RemoveDescrColumn i) "Remove from description"
                        , brightButton (CategoryColumn i) "Set as category"
                        ]

                else
                    [ Element.none ]
               )
        )


buildProfile : Model msg -> Maybe ImportProfile
buildProfile model =
    if model.profileName == "" then
        Nothing

    else
        Maybe.map3
            (\date dateFormat amount ->
                importProfile -1 model.profileName model.separator date model.descriptionColumns amount dateFormat model.categoryColumn
            )
            model.dateColumn
            model.dateFormat
            (amountField model.amountColumn)


amountField : AmountColumn -> Maybe AmountField
amountField amountColumm =
    case amountColumm of
        NoneSelected ->
            Nothing

        Simple i ->
            Just (ImportProfile.Simple i)

        CreditDebit c d ->
            Maybe.map2 ImportProfile.Split c d


amountColumnString : AmountColumn -> String
amountColumnString amountColumn =
    let
        maybe =
            Maybe.map String.fromInt >> Maybe.withDefault "None"
    in
    case amountColumn of
        NoneSelected ->
            "None"

        Simple i ->
            "Column " ++ String.fromInt i

        CreditDebit c d ->
            String.concat [ "Credit column: ", maybe c, ", ", "Debit column: ", maybe d ]
