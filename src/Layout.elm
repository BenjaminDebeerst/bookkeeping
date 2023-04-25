module Layout exposing (layout)

import Element exposing (Element, alignTop, column, el, link, padding, rgb, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


layout : String -> Element msg -> Html msg
layout title content =
    Element.layout [] <|
        row
            [ alignTop, spacing 20 ]
            [ navigation
            , column []
                [ el [ Font.bold, Font.size 20, padding 20 ] <| text title
                , content
                ]
            ]


navigation =
    column [ spacing 20, padding 10, Background.color (rgb 0.7 0.7 0.7) ]
        [ link [] { url = "/", label = text "Home" }
        , link [] { url = "/csv-import", label = text "Import" }
        , link [] { url = "/bookings", label = text "Book" }
        ]
