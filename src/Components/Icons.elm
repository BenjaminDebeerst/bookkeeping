module Components.Icons exposing (Icon, barChart, checkMark, circle, circleFill, copy, cross, edit, folder, folderPlus, infoMark, list, plusSquare, save, settings, triangleDown, triangleLeft, triangleRight, triangleUp, wand, warnTriangle, x, xSquare)

import Element exposing (Attribute, Element, el)
import FeatherIcons as Feather
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (cx, cy, d, fill, r)


type alias Icon msg =
    List (Attribute msg) -> Int -> Element msg


barChart =
    basicIcon Feather.barChart


circle : Icon msg
circle =
    basicIcon (Feather.circle |> Feather.withStrokeWidth 4)


circleFill : Icon msg
circleFill =
    basicIcon (Feather.customIcon [ Svg.circle [ fill "currentColor", cx "12", cy "12", r "10" ] [] ] |> Feather.withStrokeWidth 4)


checkMark : Icon msg
checkMark =
    basicIcon Feather.check


cross : Icon msg
cross =
    basicIcon Feather.xCircle


copy : Icon msg
copy =
    basicIcon Feather.copy


edit : Icon msg
edit =
    basicIcon Feather.edit


folder : Icon msg
folder =
    basicIcon Feather.folder


folderPlus : Icon msg
folderPlus =
    basicIcon Feather.folderPlus


infoMark : Icon msg
infoMark =
    basicIcon Feather.info


list : Icon msg
list =
    basicIcon Feather.list


plusSquare : Icon msg
plusSquare =
    basicIcon Feather.plusSquare


save : Icon msg
save =
    basicIcon Feather.save


settings =
    basicIcon Feather.settings


triangleUp attrs size =
    basicIcon Feather.triangle attrs size


triangleDown : Icon msg
triangleDown attrs size =
    el [ Element.rotate <| degrees 180 ] <| triangleUp attrs size


triangleLeft : Icon msg
triangleLeft attrs size =
    el [ Element.rotate <| degrees 270 ] <| triangleUp attrs size


triangleRight : Icon msg
triangleRight attrs size =
    el [ Element.rotate <| degrees 90 ] <| triangleUp attrs size


wand : Icon msg
wand =
    basicIcon (Feather.customIcon wandSvg |> Feather.withStrokeWidth 0.3 |> Feather.withViewBox "-0.2 -0.2 16.4 16.4")


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


warnTriangle : Icon msg
warnTriangle =
    basicIcon Feather.alertTriangle


x : Icon msg
x =
    basicIcon Feather.x


xSquare : Icon msg
xSquare =
    basicIcon Feather.xSquare


basicIcon : Feather.Icon -> Icon msgS
basicIcon icon attrs size =
    el attrs
        (Element.html
            (Feather.toHtml
                [ Attributes.height (String.fromInt size)
                , Attributes.width (String.fromInt size)
                ]
                icon
            )
        )
