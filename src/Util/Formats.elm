module Util.Formats exposing (formatDate, formatEuro, formatEuroStr, formatYearMonth)

import Config exposing (color)
import Element exposing (Element, alignRight, el, text)
import Element.Font as Font
import Time.Date as Date exposing (Date)


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
    sign ++ eur ++ "." ++ ct ++ " €"


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
    String.join "-" <| List.map String.fromInt <| [ Date.year date, Date.month date, Date.day date ]


formatYearMonth : Date -> String
formatYearMonth date =
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
