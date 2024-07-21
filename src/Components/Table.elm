module Components.Table exposing (fullStyledColumn, style, styledColumn, textColumn, withColumnWidth)

import Config exposing (color, size, style)
import Element exposing (Element, IndexedColumn, Length, clipY, el, fill, height, padding, scrollbarX, shrink, spacing, text, width)
import Element.Background as Background
import Element.Font as Font


style =
    { table = [ spacing size.tiny ]
    , fullWidthTable = [ spacing size.tiny, width fill, scrollbarX, clipY ]
    , header =
        [ Background.color color.brightAccent
        , Font.bold
        , Font.color color.black
        , Font.size size.m
        , padding size.xs
        , spacing size.xs
        , width fill
        , height fill
        ]
    , row =
        \i ->
            let
                bgColor =
                    if modBy 2 i == 1 then
                        color.white

                    else
                        color.extraBrightAccent
            in
            [ Background.color bgColor
            , height fill
            , padding size.xs
            ]
    }


textColumn : String -> (record -> String) -> IndexedColumn record msg
textColumn title textFromRow =
    styledColumn title (textFromRow >> text)


styledColumn : String -> (record -> Element msg) -> IndexedColumn record msg
styledColumn title elementFromRow =
    fullStyledColumn (text title) elementFromRow


fullStyledColumn : Element msg -> (record -> Element msg) -> IndexedColumn record msg
fullStyledColumn header elementFromRow =
    { header = el style.header <| header
    , width = shrink
    , view = \i e -> el (style.row i) <| elementFromRow e
    }


withColumnWidth : Length -> IndexedColumn record msg -> IndexedColumn record msg
withColumnWidth size indexedColumn =
    { indexedColumn | width = size }
