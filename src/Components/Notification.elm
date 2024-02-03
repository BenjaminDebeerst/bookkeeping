module Components.Notification exposing (Notification(..), showNotification)

import Config exposing (color, size)
import Element exposing (Element, padding, row, spacing)
import Element.Background as Background
import Element.Border as Border


type Notification msg
    = None
    | Info (List (Element msg))
    | Error (List (Element msg))


showNotification : Notification msg -> Element msg
showNotification n =
    case n of
        None ->
            Element.none

        Info messages ->
            row
                [ Background.color color.extraBrightAccent
                , spacing size.xs
                , padding size.xs
                , Border.width size.tiny
                , Border.color color.brightAccent
                , Border.rounded size.xs
                ]
                messages

        Error messages ->
            row
                [ Background.color color.bgRed
                , spacing size.xs
                , padding size.xs
                , Border.width size.tiny
                , Border.color color.red
                , Border.rounded size.xs
                ]
                messages
