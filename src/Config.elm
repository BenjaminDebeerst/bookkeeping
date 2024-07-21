module Config exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


paddingBottom i =
    paddingEach { top = 0, right = 0, bottom = i, left = 0 }


color =
    { black = rgb255 0x2E 0x34 0x36 -- charcoal
    , darkGrey = rgb255 65 65 65 -- dark grey
    , grey = rgb255 0xC8 0xCE 0xD0 -- light grey
    , white = rgb255 0xF6 0xF2 0xEB -- light beige
    , darkAccent = rgb255 0x05 0x87 0x89 -- blue
    , brightAccent = rgb255 0xE3 0xA7 0x2F -- yellow
    , extraBrightAccent = rgb255 0xF0 0xEC 0xC9 -- beige
    , red = rgb255 0xD5 0x4B 0x1A -- quite orange
    , bgRed = rgba255 0xD5 0x4B 0x1A 0.2
    }


size =
    { tiny = 2
    , xxs = 4
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
    , h1 = [ Font.bold, Font.size size.l, paddingBottom size.l ]
    , h2 = [ Font.bold, Font.size size.m, paddingBottom size.m ]
    , h3 = [ Font.bold, Font.size size.s, paddingBottom size.m ]
    , statusbar =
        [ width fill
        , Background.color color.black
        , Font.color color.white
        , Font.size size.s
        , padding size.xs
        , spacing size.s
        , alignBottom
        ]
    }
