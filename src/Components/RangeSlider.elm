module Components.RangeSlider exposing (Model, Selection(..), init, max, min, update, view)

{-| A slider input with two thumbs.

Elm-UI only has a slider with a single thumb. This component fakes a two-thumb slider by
placing two sliders next to one another and adjusting their sizes according to the current
selection. In operates on a discrete list of items (rather than on a continuous range).

It is not perfect and the thumbs jump around a little bit when the thumbs are reaching their maximum
positions, but it works well enough. Does not play well with too fine-grained ranges because the thumbs overlap
each other and make it hard to change focus from one slider to the other.

-}

import Components.Icons exposing (circle, circleFill)
import Components.Input exposing (button, disabledButton)
import Config exposing (color, size)
import Cons exposing (Cons)
import Element exposing (Attribute, Element, alignTop, behindContent, centerX, centerY, column, el, fill, fillPortion, height, inFront, moveLeft, px, row, shrink, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (Label, Thumb, labelHidden)


type Selection
    = All
    | Last Int
    | Range Int Int


type Side
    = Min
    | Max


type Model a
    = Model
        { options : Cons a
        , focus : Side
        , min : Int
        , max : Int
        , showQuickSelect : Bool
        }


type alias SelectedRange a =
    { min : a
    , max : a
    }


min : Model a -> a
min (Model m) =
    get m.options m.min


max : Model a -> a
max (Model m) =
    get m.options m.max


init : a -> List a -> Bool -> Selection -> Model a
init head tail showQuickSelect initialSelection =
    let
        ( lower, upper ) =
            range initialSelection (List.length tail + 1)
    in
    Model
        { options = Cons.cons head tail
        , focus = Min
        , min = lower
        , max = upper
        , showQuickSelect = showQuickSelect
        }


update : Selection -> Model a -> Model a
update msg (Model m) =
    let
        ( lower, upper ) =
            range msg (Cons.length m.options)
    in
    Model { m | min = lower, max = upper }


range : Selection -> Int -> ( Int, Int )
range selection n =
    case selection of
        All ->
            ( 0, n - 1 )

        Last count ->
            ( Basics.max 0 (n - count), n - 1 )

        Range l u ->
            ( l, u )


get : Cons a -> Int -> a
get options i =
    options |> Cons.drop i |> List.head |> Maybe.withDefault (Cons.head options)


view : String -> (a -> String) -> Model a -> Element Selection
view label format model =
    case model of
        Model m ->
            let
                n =
                    Cons.length m.options

                trackParts =
                    ( m.min, m.max - m.min + 1, n - m.max - 1 )

                pad =
                    if m.min == m.max then
                        1

                    else
                        0

                ( lSize, rSize ) =
                    case m.focus of
                        Min ->
                            ( m.max, n - m.max - pad )

                        Max ->
                            ( m.min + 1 - pad, n - m.min - 1 )

                ( lMax, rMin ) =
                    case m.focus of
                        Min ->
                            ( m.max, m.max - 1 + pad )

                        Max ->
                            ( m.min + 1 - pad, m.min )

                sliderMin =
                    slider Min lSize 0 lMax m.min (round >> (\i -> Range i m.max))

                sliderMax =
                    slider Max rSize rMin (n - 1) m.max (round >> (\i -> Range m.min i))

                btn msg s =
                    if ( m.min, m.max ) == range msg (Cons.length m.options) then
                        disabledButton s

                    else
                        button msg s
            in
            row [ width fill, spacing size.m ]
                [ el [ alignTop, width shrink ] <| text label
                , column [ width fill, spacing size.m ]
                    [ row [ width fill, spacing size.m ]
                        [ text (format <| get m.options m.min)
                        , row
                            [ width fill, height (px size.l), behindContent (track trackParts) ]
                            [ sliderMin
                            , el [ width (fillPortion pad) ] Element.none
                            , sliderMax
                            ]
                        , text (format <| get m.options m.max)
                        ]
                    , if m.showQuickSelect then
                        row [ width fill, spacing size.m ]
                            [ text "Quick select:"
                            , btn All "All"
                            , btn (Last 12) "Last Year"
                            , btn (Last 6) "Last 6 Months"
                            , btn (Last 3) "Last 3 Months"
                            , btn (Last 1) "Last Month"
                            ]

                      else
                        Element.none
                    ]
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


thumbSize =
    size.m


slider : Side -> Int -> Int -> Int -> Int -> (Float -> Selection) -> Element Selection
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
