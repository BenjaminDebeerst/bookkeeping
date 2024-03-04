module Components.Input exposing (brightButton, button)

import Config exposing (color, style)
import Element exposing (Element, text)
import Element.Background as Background
import Element.Input as Input


button : msg -> String -> Element msg
button msg label =
    Input.button style.button { onPress = Just msg, label = text label }


brightButton : msg -> String -> Element msg
brightButton msg label =
    Input.button (style.button ++ [ Background.color color.extraBrightAccent ]) { onPress = Just msg, label = text label }
