module Components.Notification exposing (Notification(..), delay, showNotification)

import Config exposing (color, size)
import Effect exposing (Effect)
import Element exposing (Element, padding, row, spacing)
import Element.Background as Background
import Element.Border as Border
import Process
import Task


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


delay : Float -> msg -> Effect msg
delay seconds msg =
    Process.sleep (seconds * 1000)
        |> Task.perform (\_ -> msg)
        |> Effect.sendCmd
