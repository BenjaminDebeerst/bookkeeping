module Pages.Home_ exposing (Model, Msg, page)

import Components.Icons as Icons
import Components.Notification as Notification exposing (Notification)
import Config exposing (color, size)
import Effect exposing (Effect)
import Element exposing (Element, centerX, centerY, column, el, height, maximum, paragraph, pointer, px, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import File exposing (File)
import File.Select as Select
import Json.Decode as Decode exposing (Error)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Shared.Model exposing (Model(..))
import Task
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> init shared
        , update = update shared
        , view = \model -> { title = "Bookkeeping", body = view shared model }
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    case shared of
        Loaded _ ->
            ( {}, Effect.pushRoutePath Path.Book )

        _ ->
            ( {}, Effect.none )


type Msg
    = InitDatabase
    | PickFile
    | GotFileName File
    | GotFileContent String String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case shared of
        Loaded _ ->
            ( {}, Effect.pushRoutePath Path.Book )

        _ ->
            case msg of
                PickFile ->
                    ( model
                    , Select.file [ "*" ] GotFileName |> Effect.sendCmd
                    )

                GotFileName file ->
                    ( {}, Effect.sendCmd <| Task.perform (GotFileContent <| File.name file) <| File.toString file )

                GotFileContent _ content ->
                    ( {}, Effect.loadDatabase content )

                InitDatabase ->
                    ( {}, Effect.truncateDatabase )



-- VIEW


view : Shared.Model -> Model -> Element Msg
view sharedModel model =
    el [ centerX, centerY ] <|
        column
            [ width <| maximum 600 shrink
            , spacing size.m
            , centerX
            , centerY
            ]
            (actionsFor sharedModel)


actionsFor sharedModel =
    case sharedModel of
        Loaded _ ->
            [ Element.none ]

        None ->
            [ showActions [ loadButton "Open a database file", initButton ] ]

        Problem error ->
            let
                errStr =
                    Decode.errorToString error

                extract =
                    if String.length errStr > 250 then
                        String.left 100 errStr
                            ++ " [...] "
                            ++ String.right 100 errStr

                    else
                        errStr
            in
            [ el [ centerX ] <| Notification.showNotification <| Notification.Error [ text "There was an issue loading the data from the DB!" ]
            , el [ centerX ] <| paragraph [] [ text extract ]
            , showActions [ loadButton "Load another DB json file", initButton ]
            ]


loadButton label =
    largeButton PickFile Icons.folder label


initButton =
    largeButton InitDatabase Icons.plusSquare "Start from scratch"


largeButton msg icon label =
    el
        [ onClick msg
        , pointer
        , width <| px 250
        , height <| px 250
        , Background.color color.extraBrightAccent
        , Border.color color.brightAccent
        , Border.width size.xxs
        , Border.rounded size.m
        , Font.color color.darkGrey
        ]
    <|
        column [ centerX, centerY ]
            [ icon [] 200
            , el [ centerX ] <| text label
            ]


showActions : List (Element Msg) -> Element Msg
showActions buttons =
    row [ Font.size size.m, spacing size.m, centerX, centerY ] buttons
