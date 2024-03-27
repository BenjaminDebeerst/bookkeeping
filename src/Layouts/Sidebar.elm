module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import Config exposing (color, size, style)
import Effect exposing (Effect)
import Element exposing (Element, column, el, fill, height, link, padding, px, row, scrollbarX, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path exposing (Path(..), toString)
import Shared
import View exposing (View)


type alias Props =
    { dataSummary : Maybe ( Int, Int, Int ) }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \_ -> ( {}, Effect.none )
        , update = \_ -> \model -> ( model, Effect.none )
        , view = view props
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    {}



-- UPDATE


type alias Msg =
    {}


update : Msg -> Model -> ( Model, Effect Msg )
update _ model =
    ( model, Effect.none )



-- VIEW


view : Props -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props { toContentMsg, model, content } =
    { title = content.title
    , body =
        row
            [ width fill, height fill, Font.size size.m ]
            [ sidebar
            , column [ height fill, width fill ]
                [ mainColumn content
                , footer props
                ]
            ]
    }


mainColumn content =
    column [ height fill, width fill, scrollbarX, padding size.l, spacing size.m, Background.color color.white ]
        [ el style.h1 (text content.title)
        , content.body
        ]


sidebar =
    column
        [ height fill
        , width (px 250)
        , padding size.l
        , scrollbarY
        , Background.color color.black
        , Font.color color.white
        , Font.size size.l
        , spacing size.m
        ]
        [ link [] { url = toString Home_, label = text "Home" }
        , link [] { url = toString ImportFile, label = text "Import File" }
        , link [] { url = toString Book, label = text "Book" }
        , link [] { url = toString Monthly, label = text "Monthly View" }
        , link [] { url = toString Accounts, label = text "Accounts" }
        , link [] { url = toString Categories, label = text "Categories" }
        , link [] { url = toString ImportProfiles, label = text "Import Profiles" }
        ]


footer : Props -> Element msg
footer props =
    Element.row style.statusbar
        [ props.dataSummary |> Maybe.map summaryString |> Maybe.withDefault "" |> text
        ]


summaryString : ( Int, Int, Int ) -> String
summaryString ( entries, accounts, categories ) =
    String.concat
        [ String.fromInt entries
        , " entries, "
        , String.fromInt accounts
        , " accounts, "
        , String.fromInt categories
        , " categories."
        ]
