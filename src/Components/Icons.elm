module Components.Icons exposing (checkMark, copy, cross, edit, folderPlus, infoMark, loader, plusSquare, triangleDown, triangleLeft, triangleRight, triangleUp, wand, warnTriangle)

import Element exposing (Attribute, Element, el)
import FeatherIcons exposing (Icon, withStrokeWidth, withViewBox)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (d, fill)


triangleUp attrs size =
    el attrs <|
        Element.html
            (FeatherIcons.triangle
                |> FeatherIcons.toHtml
                    [ Attributes.height <| String.fromInt size
                    ]
            )


triangleDown attrs size =
    el [ Element.rotate <| degrees 180 ] <| triangleUp attrs size


triangleLeft attrs size =
    el [ Element.rotate <| degrees 270 ] <| triangleUp attrs size


triangleRight attrs size =
    el [ Element.rotate <| degrees 90 ] <| triangleUp attrs size


checkMark : List (Attribute msg) -> Int -> Element msg
checkMark =
    basicIcon FeatherIcons.check


infoMark : List (Attribute msg) -> Int -> Element msg
infoMark =
    basicIcon FeatherIcons.info


warnTriangle : List (Attribute msg) -> Int -> Element msg
warnTriangle =
    basicIcon FeatherIcons.alertTriangle


copy : List (Attribute msg) -> Int -> Element msg
copy =
    basicIcon FeatherIcons.copy


folderPlus : List (Attribute msg) -> Int -> Element msg
folderPlus =
    basicIcon FeatherIcons.folderPlus


loader : List (Attribute msg) -> Int -> Element msg
loader =
    basicIcon FeatherIcons.loader


plusSquare : List (Attribute msg) -> Int -> Element msg
plusSquare =
    basicIcon FeatherIcons.plusSquare


edit : List (Attribute msg) -> Int -> Element msg
edit =
    basicIcon FeatherIcons.edit


wand =
    basicIcon (FeatherIcons.customIcon wandSvg |> withStrokeWidth 0.3 |> withViewBox "-0.2 -0.2 16.4 16.4")


wandSvg : List (Svg Never)
wandSvg =
    [ Svg.path [ fill "currentColor", d "M6.1 4.1l-2.1 2 9.8 9.9 2.2-2.1-9.9-9.8zM6.1 5.5l2.4 2.5-0.6 0.6-2.5-2.5 0.7-0.6z" ] []
    , Svg.path [ fill "currentColor", d "M0 5h3v1h-3v-1z" ] []
    , Svg.path [ fill "currentColor", d "M0.836 0.199l3.465 3.465-0.707 0.707-3.465-3.465 0.707-0.707z" ] []
    , Svg.path [ fill "currentColor", d "M10.131 0.161l0.707 0.707-2.97 2.97-0.707-0.707 2.97-2.97z" ] []
    , Svg.path [ fill "currentColor", d "M11 6h-1.5l-1-1h2.5z" ] []
    , Svg.path [ fill "currentColor", d "M3.131 7.161l0.707 0.707-2.97 2.97-0.707-0.707 2.97-2.97z" ] []
    , Svg.path [ fill "currentColor", d "M5 0h1v3h-1v-3z" ] []
    , Svg.path [ fill "currentColor", d "M6 11h-1v-2.5l1 1z" ] []
    ]


cross : List (Attribute msg) -> Int -> Element msg
cross =
    basicIcon FeatherIcons.xCircle


basicIcon : Icon -> List (Attribute msg) -> Int -> Element msg
basicIcon icon attrs size =
    el attrs
        (Element.html
            (FeatherIcons.toHtml
                [ Attributes.height (String.fromInt size)
                , Attributes.width (String.fromInt size)
                ]
                icon
            )
        )
