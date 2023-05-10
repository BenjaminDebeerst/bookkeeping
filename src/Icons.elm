module Icons exposing (triangleDown, triangleUp)

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
