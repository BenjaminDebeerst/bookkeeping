module Icons exposing (checkMark, triangleDown, triangleUp, warnTriangle)

import Element exposing (el)
import FeatherIcons
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


checkMark attrs size =
    el attrs <|
        Element.html
            (FeatherIcons.check
                |> FeatherIcons.toHtml [ Attributes.height <| String.fromInt size ]
            )


warnTriangle attrs size =
    el attrs <|
        Element.html
            (FeatherIcons.alertTriangle
                |> FeatherIcons.toHtml [ Attributes.height <| String.fromInt size ]
            )
