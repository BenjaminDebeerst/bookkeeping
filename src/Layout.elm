module Layout exposing (color, layout, size, style)

import Element exposing (Element, alignTop, column, el, fill, fillPortion, height, link, maximum, minimum, padding, paddingEach, paddingXY, rgb, rgb255, row, scrollbarX, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


layout : String -> Element msg -> Html msg
layout title contentEl =
    Element.layout [ width fill, height fill ] <|
        row
            [ width <| minimum 600 fill, height fill, Font.size size.m ]
            [ sidebar
            , content title contentEl
            ]


sidebar =
    column
        [ height fill
        , width <| fillPortion 1
        , padding size.l
        , scrollbarY
        , Background.color color.dark
        , Font.color color.light
        , Font.size size.l
        , spacing size.m
        ]
        [ link [] { url = "/", label = text "Home" }
        , link [] { url = "/csv-import", label = text "Import" }
        , link [] { url = "/bookings", label = text "Book" }
        ]


content title element =
    column [ height fill, width <| fillPortion 7, scrollbarX, padding size.l ]
        [ el [ Font.bold, Font.size size.l, paddingBottom size.l, width fill ] <| text title
        , element
        ]


paddingBottom i =
    paddingEach { top = 0, right = 0, bottom = i, left = 0 }


color =
    -- TODO make this a proper color palette
    { dark = rgb255 0x2E 0x34 0x36
    , light = rgb255 0xEE 0xEE 0xEE
    , darkAccent = rgb255 0x15 0x4C 0x79
    , lightAccent = rgb255 0x76 0xB5 0xC5
    }


size =
    { xxs = 4
    , xs = 8
    , s = 12
    , m = 16
    , l = 24
    , xl = 32
    , xxl = 64
    }


style =
    { button =
        [ paddingEach { top = size.xs, right = size.xs, bottom = size.xxs, left = size.xs }
        , Border.rounded 4
        , Border.color color.darkAccent
        , Background.color color.lightAccent
        ]
    }
