module Components.Input exposing (brightButton, button, disabledButton, largeButton)

import Components.Icons exposing (Icon)
import Config exposing (color, size, style)
import Element exposing (Element, centerX, centerY, column, el, height, pointer, px, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input


button : msg -> String -> Element msg
button msg label =
    Input.button style.button { onPress = Just msg, label = text label }


disabledButton : String -> Element msg
disabledButton label =
    Input.button (style.button ++ [ Background.color color.grey, pointer ]) { onPress = Nothing, label = text label }


brightButton : msg -> String -> Element msg
brightButton msg label =
    Input.button (style.button ++ [ Background.color color.extraBrightAccent ]) { onPress = Just msg, label = text label }


largeButton : msg -> Icon msg -> String -> Element msg
largeButton msg icon label =
    el
        [ onClick msg
        , pointer
        , width <| px 250
        , height <| px 250
        , Background.color color.extraBrightAccent
        , Border.color color.brightAccent
        , Border.width size.xxs
        , Border.rounded size.m
        , Font.color color.darkGrey
        ]
    <|
        column [ centerX, centerY ]
            [ icon [] 200
            , el [ centerX ] <| text label
            ]
