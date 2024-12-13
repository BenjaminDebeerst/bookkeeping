module Pages.Export exposing (Model, Msg, page)

import Config exposing (color, size)
import Dict
import Effect exposing (Effect)
import Element exposing (Element, el, fill, padding, text, width)
import Element.Border as Border
import Element.Font as Font
import Layouts
import Page exposing (Page)
import Persistence.Account exposing (Account, Accounts)
import Persistence.Category exposing (Categories, Category, CategoryGroup(..))
import Persistence.Data exposing (Data)
import Processing.BookEntry exposing (Categorization(..))
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (asc, bookEntryDate)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Util.Formats exposing (formatDate, formatYearMonthNumeric)
import Util.Layout exposing (dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ( (), Effect.none )
        , update = \_ _ -> ( (), Effect.none )
        , subscriptions = \_ -> Sub.none
        , view = dataView shared "Monthly" view
        }
        |> Page.withLayout (\_ -> Layouts.Tabs { dataSummary = dataSummary shared })


type alias Model =
    ()


type alias Msg =
    Never


view : Data -> Model -> Element Msg
view data _ =
    Element.column [ width fill ]
        [ el [ padding size.m ] <| text "Beancount format"
        , codeBlock
            [ options
            , accounts data.accounts
            , categories data.categories
            , entries data
            ]
        ]


options =
    [ "option \"operating_currency\" \"EUR\"" ]


accounts : Accounts -> List String
accounts =
    Dict.values
        >> List.map
            (\a ->
                let
                    name =
                        a.name |> sanitize
                in
                String.join "\n"
                    [ String.join " "
                        [ formatYearMonthNumeric a.start.yearMonth ++ "-01"
                        , "open"
                        , "Assets:" ++ name
                        ]
                    , String.join " "
                        [ formatYearMonthNumeric a.start.yearMonth ++ "-01"
                        , "open"
                        , "Equity:Opening-Balances:" ++ name
                        ]
                    , String.join " "
                        [ formatYearMonthNumeric a.start.yearMonth ++ "-01"
                        , "*"
                        , "\"Initializing bank account\""
                        ]
                    , "  " ++ "Assets:" ++ name ++ "  " ++ formatEuroStr a.start.amount
                    , "  " ++ "Equity:Opening-Balances:" ++ name
                    ]
            )


categories : Categories -> List String
categories =
    Dict.values
        >> List.map
            (\cat ->
                String.join " "
                    [ "2022-01-01 open"
                    , catAccName cat
                    ]
            )


catAccName : Category -> String
catAccName cat =
    (case cat.group of
        Income ->
            "Income:"

        Expense ->
            "Expenses:"

        Internal ->
            "Assets:"
    )
        ++ cat.name
        |> sanitize


entries data =
    getEntries data [] (asc bookEntryDate)
        |> List.map
            (\be ->
                String.join "\n"
                    ([ String.join " "
                        [ formatDate be.date
                        , "*"
                        , "\"" ++ (be.description |> String.replace "\n" " ") ++ "\""
                        ]
                     , "  " ++ "Assets:" ++ (be.account.name |> sanitize) ++ "  " ++ formatEuroStr be.amount
                     ]
                        ++ (case be.categorization of
                                None ->
                                    [ "  FIXME:Uncategorized" ]

                                Single cat ->
                                    [ "  " ++ catAccName cat ]

                                Split pieces ->
                                    pieces |> List.map (\p -> "  " ++ catAccName p.category ++ "  " ++ formatEuroStr -p.amount)
                           )
                    )
            )


codeBlock : List (List String) -> Element msg
codeBlock elements =
    let
        s =
            elements |> List.map (String.join "\n") |> String.join "\n\n"
    in
    el [ padding size.m, width fill ] (el [ padding size.m, Border.width size.tiny, Border.color color.black, Font.family [ Font.monospace ], width fill ] (text s))


sanitize : String -> String
sanitize s =
    String.split " " s
        |> List.map
            (String.replace " " ":"
                >> String.replace "(" ""
                >> String.replace ")" ""
                >> String.replace "/" "-"
                >> String.replace "ä" "ae"
                >> String.replace "ü" "ue"
                >> String.replace "ö" "oe"
                >> capitalize
            )
        |> String.join ":"


capitalize : String -> String
capitalize word =
    case String.uncons word of
        Nothing ->
            ""

        Just ( h, t ) ->
            String.cons (Char.toUpper h) t


formatEuroStr : Int -> String
formatEuroStr cents =
    let
        sign =
            if cents < 0 then
                "-"

            else
                ""

        str =
            String.fromInt (abs cents)

        ct =
            String.padLeft 2 '0' (String.right 2 str)

        eur =
            String.padLeft 1 '0' (String.slice 0 -2 str)
    in
    sign ++ eur ++ "." ++ ct ++ " EUR"
