module Components.Graph exposing (Datum, view)

import Dict exposing (Dict)
import Element exposing (Element, el, fill, height, htmlAttribute, width)
import Html.Attributes exposing (id)
import Svg
import Svg.Attributes
import Util.YearMonth exposing (YearMonth)


type alias Datum =
    { month : YearMonth
    , values : Dict String Int
    }


view : List Datum -> ( Float, Float ) -> Element msg
view data ( x, y ) =
    el [ htmlAttribute (id "graph"), width fill, height fill ] <|
        (Svg.svg
            [ Svg.Attributes.width (String.fromFloat x)
            , Svg.Attributes.height (String.fromFloat y)
            , Svg.Attributes.viewBox "-2 -2 4 4"
            , Html.Attributes.style "background" "yellow"
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r "1"
                , Svg.Attributes.fill "blue"
                ]
                []
            ]
            |> Element.html
        )
