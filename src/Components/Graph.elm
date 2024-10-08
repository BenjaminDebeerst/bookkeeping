module Components.Graph exposing (Datum, Model, Msg(..), init, update, view)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI exposing (Bar)
import Dict exposing (Dict)
import Element exposing (Element)
import Util.Formats as Formats
import Util.YearMonth as YearMonth exposing (YearMonth)


type alias Datum =
    { month : YearMonth
    , values : Dict String Int
    }


type alias Model =
    { width : Float
    , height : Float
    , hovering : List (CI.Many Datum CI.Any)
    }


init : Model
init =
    { width = 0
    , height = 0
    , hovering = []
    }


type Msg
    = SetDimensions Float Float
    | OnHover (List (CI.Many Datum CI.Any))


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetDimensions w h ->
            { model | width = w, height = h }

        OnHover hovering ->
            { model | hovering = hovering }


view : Model -> List Datum -> Element Msg
view model data =
    Element.html <|
        C.chart (attributes model) (labels model ++ axis ++ bars data ++ events model ++ legend)


val : Datum -> String -> Maybe Float
val d s =
    Dict.get s d.values
        |> Maybe.andThen
            (\i ->
                if i == 0 then
                    Nothing

                else
                    Just (toFloat i)
            )


euro : Float -> String
euro =
    round >> Formats.formatEuroStr


attributes model =
    [ CA.height model.height
    , CA.width model.width
    , CA.margin { top = 20, bottom = 100, left = 150, right = 20 }
    , CE.onMouseMove OnHover (CE.getNearest CI.bins)
    , CE.onMouseLeave (OnHover [])
    ]


bars data =
    let
        orderedData =
            data |> List.sortBy (.month >> YearMonth.components)

        series : List String
        series =
            orderedData
                |> List.head
                |> Maybe.map (.values >> Dict.toList)
                |> Maybe.withDefault []
                |> List.sortBy Tuple.second
                |> List.map Tuple.first

        barStyles : List (C.Property Datum inter Bar)
        barStyles =
            series |> List.map (\s -> C.barMaybe (\d -> val d s) [] |> C.format euro |> C.named s)
    in
    [ C.bars [ CA.spacing 0 ] barStyles orderedData
    , C.barLabels [ CA.moveDown 6, CA.color "black", CA.format (\bar -> euro (CI.getY bar)), CA.fontSize 12 ]
    ]


labels model =
    [ C.yLabels [ CA.withGrid, CA.format euro ], C.binLabels (.month >> Formats.formatYearMonthNumeric) [ CA.moveUp (model.height / 5 * 3) ] ]


axis =
    [ C.yAxis [], C.xAxis [] ]


events model =
    [ C.each model.hovering <| \plane item -> [ C.tooltip item [] [] [] ] ]


legend =
    [ C.legendsAt .min .min [ CA.moveDown 50, CA.alignLeft, CA.spacing 20 ] [ CA.spacing 10 ] ]
