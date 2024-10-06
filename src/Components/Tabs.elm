module Components.Tabs exposing (Handle, tabbedContent)

import Components.Icons as Icon exposing (Icon)
import Config exposing (color, size)
import Element exposing (Attribute, Element, alignBottom, alignLeft, alignTop, centerY, column, el, fill, height, mouseOver, padding, paddingEach, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font


tabbedContent :
    { allTabs : List t
    , selectedTab : t
    , tabTitles : t -> String
    , tabIcons : t -> Maybe (Icon msg)
    , tabMsg : t -> msg
    , content : Element msg
    , rightCorner : List (Handle msg)
    }
    -> Element msg
tabbedContent props =
    column
        [ height fill, width fill ]
        [ row
            [ alignLeft, alignTop, width fill, height shrink, Background.color color.black, paddingEach { left = 0, right = 0, bottom = 0, top = size.xs } ]
            ((props.allTabs
                |> List.map (toHandle props.selectedTab props.tabTitles props.tabIcons props.tabMsg >> viewHandle)
             )
                ++ [ el [ width fill ] Element.none ]
                ++ (props.rightCorner |> List.map viewHandle)
            )
        , el contentBoxStyle props.content
        ]


toHandle : t -> (t -> String) -> (t -> Maybe (Icon msg)) -> (t -> msg) -> t -> Handle msg
toHandle selected descr icon msg tab =
    Handle (msg tab) (Just (descr tab)) (tab == selected) (icon tab)


type alias Handle msg =
    { action : msg
    , label : Maybe String
    , selected : Bool
    , icon : Maybe (Icon.Icon msg)
    }


viewHandle : Handle msg -> Element msg
viewHandle handle =
    el (tabStyle handle.selected handle.action) <|
        row [ spacing size.s ]
            [ handle.icon |> Maybe.map (\i -> i [ centerY ] size.l) |> Maybe.withDefault Element.none
            , handle.label |> Maybe.map (\s -> el [ alignBottom ] <| text s) |> Maybe.withDefault Element.none
            ]



-- STYLE


contentBoxStyle : List (Attribute msg)
contentBoxStyle =
    [ spacing size.m
    , width fill
    , height fill
    , Background.color color.white
    ]


tabStyle : Bool -> msg -> List (Attribute msg)
tabStyle selected targetMsg =
    [ Font.bold
    , onClick targetMsg
    , pointer
    , paddingEach { left = size.l, top = size.s, right = size.l, bottom = size.s }
    , Border.widthEach { left = 0, top = 0, right = 0, bottom = size.xxs }
    , mouseOver [ Border.color color.brightAccent ]
    , alignBottom
    ]
        ++ (if selected then
                [ Background.color color.white, Border.color color.white, Font.color color.black ]

            else
                [ Background.color color.black, Border.color color.black, Font.color color.white ]
           )
