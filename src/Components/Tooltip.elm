module Components.Tooltip exposing (tooltip)

import Config exposing (color, size)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


tooltip : (Element msg -> Attribute msg) -> String -> Attribute msg
tooltip usher tooltipContent =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << Element.map never) <|
                column []
                    [ el [ padding size.xxs ] Element.none -- a little spacer so the tooltip doesn't stick directly to the element
                    , el
                        [ htmlAttribute (Html.Attributes.style "pointerEvents" "none")
                        , Background.color color.black
                        , Font.color color.white
                        , padding size.s
                        , Border.rounded size.xs
                        , Font.size size.m
                        , Border.shadow
                            { offset = ( 0, size.xs ), blur = size.s, size = 0, color = rgba 0 0 0 0.32 }
                        ]
                        (text tooltipContent)
                    ]
            ]
            none
