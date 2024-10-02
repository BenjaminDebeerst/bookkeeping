module Components.RangeSlider exposing (Model, Msg, init, max, min, update, view)

{-| A slider input with two thumbs.

Elm-UI only has a slider with a single thumb. This component fakes a two-thumb slider by
placing two sliders next to one another and adjusting their sizes according to the current
selection. In operates on a discrete list of items (rather than on a continuous range).

It is not perfect and the thumbs jump around a little bit when the thumbs are reaching their maximum
positions, but it works well enough. Does not play well with too fine-grained ranges because the thumbs overlap
each other and make it hard to change focus from one slider to the other.

-}

import Components.Icons exposing (circle, circleFill)
import Config exposing (color, size)
import Cons exposing (Cons)
import Element exposing (Attribute, Element, behindContent, centerX, centerY, el, fill, fillPortion, height, inFront, moveLeft, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (Label, Thumb, labelHidden)


type Msg a
    = SetMin Int a
    | SetMax Int a


type Side
    = Min
    | Max


type Model a
    = Model
        { options : Cons a
        , min : a
        , max : a
        , iMin : Int
        , iMax : Int
        , focus : Side
        , optionCount : Int
        }


min : Model a -> a
min model =
    case model of
        Model m ->
            m.min


max : Model a -> a
max model =
    case model of
        Model m ->
            m.max


init : a -> List a -> Model a
init head tail =
    let
        options =
            Cons.cons head tail

        size =
            Cons.length options
    in
    Model
        { options = options
        , min = head
        , max = Cons.reverse options |> Cons.head
        , iMin = 0
        , iMax = size - 1
        , focus = Min
        , optionCount = size
        }


update : Msg a -> Model a -> Model a
update msg model =
    case ( msg, model ) of
        ( SetMin i a, Model m ) ->
            Model { m | min = a, iMin = i, focus = Min }

        ( SetMax i a, Model m ) ->
            Model { m | max = a, iMax = i, focus = Max }


view : String -> (a -> String) -> Model a -> Element (Msg a)
view label format model =
    case model of
        Model m ->
            let
                trackParts =
                    ( m.iMin, m.iMax - m.iMin + 1, m.optionCount - m.iMax - 1 )

                pad =
                    if m.iMin == m.iMax then
                        1

                    else
                        0

                ( lSize, rSize ) =
                    case m.focus of
                        Min ->
                            ( m.iMax, m.optionCount - m.iMax - pad )

                        Max ->
                            ( m.iMin + 1 - pad, m.optionCount - m.iMin - 1 )

                ( lMax, rMin ) =
                    case m.focus of
                        Min ->
                            ( m.iMax, m.iMax - 1 + pad )

                        Max ->
                            ( m.iMin + 1 - pad, m.iMin )

                sliderMin =
                    slider Min lSize 0 lMax m.iMin (onChange m.options SetMin)

                sliderMax =
                    slider Max rSize rMin (m.optionCount - 1) m.iMax (onChange m.options SetMax)
            in
            row [ width fill, spacing size.m ]
                [ text label
                , text (format m.min)
                , row
                    [ width fill, height (px size.l), behindContent (track trackParts) ]
                    [ sliderMin
                    , el [ width (fillPortion pad), height fill ] Element.none
                    , sliderMax
                    ]
                , text (format m.max)
                ]


type alias Slider msg =
    { onChange : Float -> msg
    , label : Label msg
    , min : Float
    , max : Float
    , value : Float
    , thumb : Thumb
    , step : Maybe Float
    }


onChange : Cons a -> (Int -> a -> Msg a) -> (Float -> Msg a)
onChange values msg selection =
    let
        i =
            round selection

        a =
            Cons.take (i + 1) values |> List.reverse |> List.head |> Maybe.withDefault (Cons.head values)
    in
    msg i a


thumbSize =
    size.m


slider : Side -> Int -> Int -> Int -> Int -> (Float -> Msg a) -> Element (Msg a)
slider side n minIdx maxIdx valIdx onChangeMsg =
    let
        ( label, handle ) =
            case side of
                Min ->
                    ( "Min"
                    , circle [ moveLeft (thumbSize / 2), Font.color color.brightAccent ] thumbSize
                    )

                Max ->
                    ( "Max"
                    , circle [ moveLeft (thumbSize / 2), Font.color color.brightAccent ] thumbSize
                    )
    in
    Input.slider [ width (fillPortion n), height fill, centerY ]
        { onChange = onChangeMsg
        , label = labelHidden label
        , min = toFloat minIdx
        , max = toFloat maxIdx
        , value = toFloat valIdx
        , thumb = Input.thumb [ width (px 0), height (px size.m), inFront handle ]
        , step = Just 1
        }


track ( a, b, c ) =
    let
        dot clr =
            el [ width (px 0) ] <| circleFill [ centerX, Font.color clr ] size.s

        dots =
            []
                ++ List.repeat a (dot color.grey)
                ++ List.repeat (b + 1) (dot color.darkAccent)
                ++ List.repeat c (dot color.grey)

        bar size clr =
            el [ width (fillPortion size), height fill, Background.color clr ] Element.none

        bars =
            row [ width fill, height (px 3), centerY ] [ bar a color.grey, bar b color.brightAccent, bar c color.grey ]
    in
    row [ width fill, height fill, behindContent bars, spaceEvenly ] dots
