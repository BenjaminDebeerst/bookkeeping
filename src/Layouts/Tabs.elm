module Layouts.Tabs exposing (..)

import Components.Tabs
import Config exposing (size, style)
import Effect exposing (Effect)
import Element exposing (Element, column, el, fill, height, paddingEach, text, width)
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path as Pages exposing (Path(..))
import Shared
import View exposing (View)


type alias Props =
    { dataSummary : Maybe ( Int, Int, Int ) }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \_ -> ( {}, Effect.none )
        , update = update
        , view = view props route.path
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


type Msg
    = ChangeTo Path


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeTo path ->
            ( model, Effect.pushRoutePath path )


view : Props -> Path -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props path { toContentMsg, model, content } =
    { title = content.title
    , body =
        column [ height fill, width fill ]
            [ el [ paddingEach { top = 0, bottom = 0, right = 0, left = size.s }, height fill, width fill ] <| mainColumn content path toContentMsg
            , footer props
            ]
    }


pageTitles : Path -> String
pageTitles path =
    case path of
        Home_ ->
            "Home"

        Book ->
            "Book"

        ImportFile ->
            "Import File"

        Monthly ->
            "Monthly"

        Settings ->
            "Settings"

        _ ->
            "Other"


mainColumn : View contentMsg -> Path -> (Msg -> contentMsg) -> Element contentMsg
mainColumn content path toContentMsg =
    Components.Tabs.tabbedContent
        { allTabs = [ Pages.Home_, Pages.ImportFile, Pages.Book, Pages.Monthly, Pages.Settings ]
        , selectedTab = path
        , tabTitles = pageTitles
        , tabMsg = ChangeTo >> toContentMsg
        , content = content.body
        }


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
