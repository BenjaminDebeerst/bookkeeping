module Components.Table exposing (fullStyledColumn, style, styledColumn, textColumn, withColumnWidth, withHeaderActions)

import Config exposing (color, size, style)
import Element exposing (Element, IndexedColumn, Length, clipY, el, fill, height, padding, paddingXY, row, scrollbarX, shrink, spacing, text, width)
import Element.Background as Background
import Element.Font as Font


style =
    { table = [ spacing size.tiny ]
    , fullWidthTable = [ spacing size.tiny, width fill, scrollbarX, clipY ]
    , header = [ padding size.xs, spacing size.xs ] ++ headerColors
    , headerColors = headerColors
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


headerColors =
    [ Background.color color.brightAccent
    , Font.bold
    , Font.color color.black
    , Font.size size.m
    , width fill
    , height fill
    ]


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


withHeaderActions : List (Element msg) -> IndexedColumn record msg -> IndexedColumn record msg
withHeaderActions actions column =
    { header =
        row style.headerColors
            [ column.header
            , row [ paddingXY size.xs 0 ] actions
            ]
    , width = column.width
    , view = column.view
    }
