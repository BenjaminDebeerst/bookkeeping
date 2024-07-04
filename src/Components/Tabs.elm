module Components.Tabs exposing (tabbedContent)

import Config exposing (color, size)
import Element exposing (Attribute, Element, alignLeft, alignTop, column, el, fill, height, padding, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font


tabbedContent :
    { allTabs : List t
    , selectedTab : t
    , tabTitles : t -> String
    , tabMsg : t -> msg
    , content : Element msg
    }
    -> Element msg
tabbedContent props =
    column
        [ height fill, width fill ]
        [ row
            [ alignLeft, alignTop, width fill, height shrink, Background.color color.black ]
            (List.map (singleTab props.selectedTab props.tabTitles props.tabMsg) props.allTabs)
        , el contentBoxStyle props.content
        ]


singleTab : t -> (t -> String) -> (t -> msg) -> t -> Element msg
singleTab selected descr msg tab =
    el (tabStyle (tab == selected) (msg tab)) <| text (descr tab)



-- STYLE


contentBoxStyle : List (Attribute msg)
contentBoxStyle =
    [ spacing size.m
    , width fill
    , height fill
    , padding size.m
    , Background.color color.white
    ]


tabStyle : Bool -> msg -> List (Attribute msg)
tabStyle selected targetMsg =
    [ Font.bold, onClick targetMsg, padding size.m ]
        ++ (if selected then
                [ Background.color color.white, Font.color color.black ]

            else
                [ Background.color color.black, Font.color color.white ]
           )
