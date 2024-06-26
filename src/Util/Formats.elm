module Util.Formats exposing (formatDate, formatEuro, formatEuroStr, formatYearMonth, formatYearMonthNumeric)

import Config exposing (color)
import Element exposing (Element, alignRight, el, text)
import Element.Font as Font
import Time.Date as Date exposing (Date)
import Util.YearMonth as YearMonth exposing (YearMonth)


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
                |> insertThousandsCommas
    in
    sign ++ eur ++ "." ++ ct ++ " €"


insertThousandsCommas : String -> String
insertThousandsCommas s =
    if String.length s > 3 then
        insertThousandsCommas (String.dropRight 3 s) ++ "," ++ String.right 3 s

    else
        s


formatEuro : Int -> Element msg
formatEuro cents =
    let
        formatted =
            formatEuroStr cents

        styleAttrs =
            if cents < 0 then
                [ alignRight, Font.color color.red ]

            else
                [ alignRight ]
    in
    el styleAttrs (text formatted)


formatDate : Date -> String
formatDate date =
    String.join "-" <| List.map (String.fromInt >> String.padLeft 2 '0') <| [ Date.year date, Date.month date, Date.day date ]


formatYearMonthNumeric : YearMonth -> String
formatYearMonthNumeric yearMonth =
    let
        ( year, month ) =
            YearMonth.components yearMonth
    in
    String.join "-" [ String.fromInt year, String.padLeft 2 '0' <| String.fromInt <| month ]


formatYearMonth : YearMonth -> String
formatYearMonth yearMonth =
    let
        date =
            YearMonth.toDate yearMonth
    in
    String.join " "
        [ case Date.month date of
            1 ->
                "January"

            2 ->
                "February"

            3 ->
                "March"

            4 ->
                "April"

            5 ->
                "May"

            6 ->
                "June"

            7 ->
                "July"

            8 ->
                "August"

            9 ->
                "September"

            10 ->
                "October"

            11 ->
                "November"

            12 ->
                "December"

            i ->
                "Month " ++ String.fromInt i
        , String.fromInt <| Date.year date
        ]
