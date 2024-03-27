module Components.Table exposing (fullStyledColumn, styledColumn, tableStyle, textColumn, width)

import Config exposing (size, style)
import Element exposing (Element, IndexedColumn, Length, el, shrink, spacing, text)


tableStyle =
    [ spacing size.tiny ]


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


width : Length -> IndexedColumn record msg -> IndexedColumn record msg
width size indexedColumn =
    { indexedColumn | width = size }
