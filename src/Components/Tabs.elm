module Components.Tabs exposing (tabbedContent)

import Config exposing (color, size)
import Element exposing (Attribute, Element, alignLeft, centerX, centerY, column, el, fill, height, paddingEach, row, spacing, text, width)
import Element.Border as Border
import Element.Events exposing (onClick)


tabbedContent :
    { allTabs : List t
    , selectedTab : t
    , tabTitles : t -> String
    , tabMsg : t -> msg
    , content : List (Element msg)
    }
    -> Element msg
tabbedContent props =
    column
        []
        [ row
            [ alignLeft, centerY, width fill ]
            (List.map (singleTab props.selectedTab props.tabTitles props.tabMsg) props.allTabs
                ++ [ el fillTabStyle Element.none ]
            )
        , column contentBoxStyle props.content
        ]


singleTab : t -> (t -> String) -> (t -> msg) -> t -> Element msg
singleTab selected descr msg tab =
    let
        isSelected =
            tab == selected
    in
    el (tabStyle isSelected (msg tab)) <|
        el (tabTitleStyle isSelected) <|
            text (descr tab)



-- STYLE


contentBoxStyle : List (Attribute msg)
contentBoxStyle =
    [ spacing size.m
    , Border.widthEach { left = size.tiny, top = 0, right = 0, bottom = 0 }
    , Border.color color.darkAccent
    , paddingEach { left = size.m, top = size.m, right = 0, bottom = 0 }
    ]


borderWidth =
    size.tiny


tabStyle : Bool -> msg -> List (Attribute msg)
tabStyle selected targetMsg =
    if selected then
        [ Border.widthEach { left = borderWidth, top = borderWidth, right = borderWidth, bottom = 0 }
        , Border.roundEach { topLeft = size.m, topRight = size.m, bottomLeft = 0, bottomRight = 0 }
        , Border.color color.darkAccent
        , onClick targetMsg
        ]

    else
        [ Border.widthEach { bottom = borderWidth, top = 0, left = 0, right = 0 }
        , Border.roundEach { topLeft = size.m, topRight = size.m, bottomLeft = 0, bottomRight = 0 }
        , Border.color color.darkAccent
        , onClick targetMsg
        ]


fillTabStyle : List (Attribute msg)
fillTabStyle =
    [ Border.widthEach { bottom = borderWidth, top = 0, left = 0, right = 0 }
    , Border.color color.darkAccent
    , width fill
    , height fill
    ]


tabTitleStyle : Bool -> List (Attribute msg)
tabTitleStyle selected =
    let
        offset =
            if selected then
                0

            else
                borderWidth
    in
    [ centerX
    , centerY
    , paddingEach { left = size.l, right = size.l, top = size.s + offset, bottom = size.s - offset }
    ]
