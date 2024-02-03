module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import Config exposing (color, size, style)
import Effect exposing (Effect)
import Element exposing (column, el, fill, fillPortion, height, link, minimum, padding, row, scrollbarX, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path exposing (Path(..), toString)
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \_ -> ( {}, Effect.none )
        , update = \_ -> \model -> ( model, Effect.none )
        , view = view
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


view : { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view { toContentMsg, model, content } =
    { title = content.title
    , body =
        row
            [ width (minimum 600 fill), height fill, Font.size size.m ]
            [ sidebar
            , mainColumn content
            ]
    }


mainColumn content =
    column [ height fill, width (fillPortion 7), scrollbarX, padding size.l, spacing size.m, Background.color color.white ]
        [ el style.h1 (text content.title)
        , content.body
        ]


sidebar =
    column
        [ height fill
        , width <| fillPortion 1
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
