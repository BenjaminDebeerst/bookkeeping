module Components.Input exposing (brightButton, button, disabledButton)

import Config exposing (color, style)
import Element exposing (Element, text)
import Element.Background as Background
import Element.Input as Input


button : msg -> String -> Element msg
button msg label =
    Input.button style.button { onPress = Just msg, label = text label }


disabledButton : String -> Element msg
disabledButton label =
    Input.button (style.button ++ [ Background.color color.grey ]) { onPress = Nothing, label = text label }


brightButton : msg -> String -> Element msg
brightButton msg label =
    Input.button (style.button ++ [ Background.color color.extraBrightAccent ]) { onPress = Just msg, label = text label }
