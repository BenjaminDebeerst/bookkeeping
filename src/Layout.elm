module Layout exposing (color, layout, size, style)

import Element exposing (Element, alignTop, column, el, fill, fillPortion, height, link, maximum, minimum, padding, paddingEach, paddingXY, rgb, rgb255, row, scrollbarX, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


layout : String -> Element msg -> Html msg
layout title pageContent =
    Element.layout [ width fill, height fill ] <|
        row
            [ width <| minimum 600 fill, height fill, Font.size size.m ]
            [ sidebar
            , content title pageContent
            ]


sidebar =
    column
        [ height fill
        , width <| fillPortion 1
        , padding size.l
        , scrollbarY
        , Background.color color.black
        , Font.color color.white
        , Font.size size.l
        , spacing size.m
        ]
        [ link [] { url = "/", label = text "Home" }
        , link [] { url = "/csv-import", label = text "Import" }
        , link [] { url = "/import-file", label = text "Import File" }
        , link [] { url = "/bookings", label = text "Book" }
        ]


content title pageContent =
    column [ height fill, width <| fillPortion 7, scrollbarX, padding size.l, Background.color color.white ]
        [ el [ Font.bold, Font.size size.l, paddingBottom size.l, width fill ] <| text title
        , pageContent
        ]


paddingBottom i =
    paddingEach { top = 0, right = 0, bottom = i, left = 0 }


color =
    { black = rgb255 0x2E 0x34 0x36 -- charcoal
    , white = rgb255 0xF6 0xF2 0xEB -- light beige
    , extraDarkAccent = rgb255 0x50 0x3D 0x2E -- brown
    , darkAccent = rgb255 0x05 0x87 0x89 -- blue
    , brightAccent = rgb255 0xE3 0xA7 0x2F -- yellow
    , extraBrightAccent = rgb255 0xF0 0xEC 0xC9 -- beige
    , red = rgb255 0xD5 0x4B 0x1A -- quite orange
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
        , Background.color color.brightAccent
        ]
    , contentSpacing = spacing size.m
    }
