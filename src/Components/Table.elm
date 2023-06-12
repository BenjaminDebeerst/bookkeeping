module Components.Table exposing (styledColumn, tableStyle, textColumn)

import Components.Layout exposing (size, style)
import Element exposing (Element, IndexedColumn, el, shrink, spacing, text)


tableStyle =
    [ spacing size.tiny ]


textColumn : String -> (record -> String) -> IndexedColumn record msg
textColumn title textFromRow =
    { header = el style.header <| text title
    , width = shrink
    , view = \i e -> el (style.row i) <| text <| textFromRow e
    }


styledColumn : String -> (record -> Element msg) -> IndexedColumn record msg
styledColumn title elementFromRow =
    { header = el style.header <| text title
    , width = shrink
    , view = \i e -> el (style.row i) <| elementFromRow e
    }
