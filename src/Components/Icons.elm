module Components.Icons exposing (checkMark, copy, database, triangleDown, triangleUp, warnTriangle)

import Element exposing (Attribute, Element, el)
import FeatherIcons exposing (Icon)
import Svg.Attributes as Attributes


triangleUp attrs size =
    el attrs <|
        Element.html
            (FeatherIcons.triangle
                |> FeatherIcons.toHtml
                    [ Attributes.fill "fill"
                    , Attributes.height <| String.fromInt size
                    ]
            )


triangleDown attrs size =
    el [ Element.rotate <| degrees 180 ] <| triangleUp attrs size


checkMark : List (Attribute msg) -> Int -> Element msg
checkMark =
    basicIcon FeatherIcons.check


warnTriangle : List (Attribute msg) -> Int -> Element msg
warnTriangle =
    basicIcon FeatherIcons.alertTriangle


copy : List (Attribute msg) -> Int -> Element msg
copy =
    basicIcon FeatherIcons.copy


database : List (Attribute msg) -> Int -> Element msg
database =
    basicIcon FeatherIcons.database


basicIcon : Icon -> List (Attribute msg) -> Int -> Element msg
basicIcon icon attrs size =
    el attrs <|
        Element.html
            (icon
                |> FeatherIcons.toHtml [ Attributes.height <| String.fromInt size ]
            )
