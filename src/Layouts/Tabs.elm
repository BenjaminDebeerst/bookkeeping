module Layouts.Tabs exposing (..)

import Components.Icons as Icons
import Components.Tabs
import Config exposing (style)
import Effect exposing (Effect)
import Element exposing (Attribute, Element, column, el, fill, height, text, width)
import File.Download as Download
import Json.Encode
import Layout exposing (Layout)
import Persistence.Data as Data exposing (Data)
import Route exposing (Route)
import Route.Path as Path exposing (..)
import Shared
import Util.Layout exposing (dataUpdate)
import View exposing (View)


type alias Props =
    { dataSummary : Maybe ( Int, Int, Int ) }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \_ -> ( {}, Effect.none )
        , update = dataUpdate shared update
        , view = view props route.path
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


type Msg
    = ChangeTo Path
    | Save
    | Close


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        ChangeTo path ->
            ( model, Effect.pushRoutePath path )

        Save ->
            ( model, Effect.sendCmd <| Download.string "bookkeeping.json" "application/json" (Json.Encode.encode 0 (Data.jsonEncoder data)) )

        Close ->
            ( model, Effect.closeDatabase )


view : Props -> Path -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props path { toContentMsg, model, content } =
    { title = content.title
    , body =
        column [ height fill, width fill ]
            [ el [ height fill, width fill ] <| mainColumn content path toContentMsg
            , footer props
            ]
    }


pageTitles : Path -> String
pageTitles path =
    case path of
        Book ->
            "Data"

        Monthly ->
            "Aggregation"

        Settings ->
            "Settings"

        _ ->
            "Other"


pageIcons path =
    case path of
        Book ->
            Just Icons.rows

        Monthly ->
            Just Icons.barChart

        Settings ->
            Just Icons.settings

        _ ->
            Nothing


mainColumn : View contentMsg -> Path -> (Msg -> contentMsg) -> Element contentMsg
mainColumn content path toContentMsg =
    Components.Tabs.tabbedContent
        { allTabs = [ Path.Book, Path.Monthly, Path.Settings ]
        , selectedTab = path
        , tabTitles = pageTitles
        , tabIcons = pageIcons
        , tabMsg = ChangeTo >> toContentMsg
        , content = content.body
        , rightCorner =
            [ Components.Tabs.Handle (toContentMsg Save) "Save" False (Just Icons.save)
            , Components.Tabs.Handle (toContentMsg Close) "Close" False (Just Icons.x)
            ]
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
