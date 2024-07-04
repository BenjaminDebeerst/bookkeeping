module Components.Tabs exposing (Handle, tabbedContent)

import Config exposing (color, size)
import Element exposing (Attribute, Element, alignLeft, alignRight, alignTop, column, el, fill, height, link, mouseOver, padding, paddingEach, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font


tabbedContent :
    { allTabs : List t
    , selectedTab : t
    , tabTitles : t -> String
    , tabMsg : t -> msg
    , content : Element msg
    , rightCorner : List (Handle msg)
    }
    -> Element msg
tabbedContent props =
    column
        [ height fill, width fill ]
        [ row
            [ alignLeft, alignTop, width fill, height shrink, Background.color color.black ]
            ((props.allTabs
                |> List.map (toHandle props.selectedTab props.tabTitles props.tabMsg >> viewHandle)
             )
                ++ [ el [ width fill ] Element.none ]
                ++ (props.rightCorner |> List.map viewHandle)
            )
        , el contentBoxStyle props.content
        ]


toHandle : t -> (t -> String) -> (t -> msg) -> t -> Handle msg
toHandle selected descr msg tab =
    Handle (msg tab) (descr tab) (tab == selected)


type alias Handle msg =
    { action : msg
    , label : String
    , selected : Bool
    }


viewHandle : Handle msg -> Element msg
viewHandle handle =
    el (tabStyle handle.selected handle.action) <| text handle.label



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
    [ Font.bold
    , onClick targetMsg
    , pointer
    , paddingEach { left = size.l, top = size.m, right = size.l, bottom = size.s - size.xxs }
    , Border.widthEach { left = 0, top = 0, right = 0, bottom = size.xxs }
    , mouseOver [ Border.color color.brightAccent ]
    ]
        ++ (if selected then
                [ Background.color color.white, Border.color color.white, Font.color color.black ]

            else
                [ Background.color color.black, Border.color color.black, Font.color color.white ]
           )
