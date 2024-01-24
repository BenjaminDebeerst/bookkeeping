module Pages.Home_ exposing (Model, Msg, page)

import Components.Icons exposing (loader)
import Components.Layout as Layout exposing (color, size)
import Components.Notification as Notification exposing (Notification)
import Dict
import Effect exposing (Effect)
import Element exposing (Element, el, fill, minimum, row, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Json.Decode as Decode exposing (Error)
import Json.Encode
import Page exposing (Page)
import Persistence.Data as Data exposing (Data)
import Route exposing (Route)
import Shared
import Shared.Model exposing (Model(..))
import Task
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { notification : Notification Msg
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( emptyModel, Effect.none )


emptyModel =
    { notification = Notification.None }



-- UPDATE


type Msg
    = InitDatabase
    | PickFile
    | GotFileName File
    | GotFileContent String String
    | SaveDataBase


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update sharedModel msg model =
    case msg of
        PickFile ->
            ( model
            , Select.file [ "*" ] GotFileName |> Effect.sendCmd
            )

        GotFileName file ->
            ( { model | notification = Notification.Info [ loader [ Font.color color.black ] size.m, text "Loading..." ] }, readFile file )

        GotFileContent name content ->
            load name content

        InitDatabase ->
            ( emptyModel, Effect.truncateDatabase )

        SaveDataBase ->
            ( model, save sharedModel )



-- MSG processing


readFile : File -> Effect Msg
readFile file =
    Effect.sendCmd <| Task.perform (GotFileContent <| File.name file) <| File.toString file


load : String -> String -> ( Model, Effect Msg )
load fileName content =
    ( { emptyModel | notification = Notification.Info [ text <| "Loaded " ++ fileName ] }
    , Effect.loadDatabase content
    )


save : Shared.Model -> Effect Msg
save model =
    case model of
        None ->
            Effect.none

        Loaded data ->
            Download.string "bookkeeping.json" "application/json" (data |> Data.jsonEncoder |> Json.Encode.encode 0) |> Effect.sendCmd

        Problem _ ->
            Effect.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view sharedModel model =
    Layout.page "Home" <|
        [ Notification.showNotification model.notification ]
            ++ (case sharedModel of
                    None ->
                        showStart

                    Loaded data ->
                        showDataSummary data

                    Problem e ->
                        showDataIssues e
               )


showStart : List (Element Msg)
showStart =
    [ el [] <| text "Welcome to Bookkeeping. What do you want to do?"
    , showActions [ Load, Init ]
    ]


showDataSummary : Data -> List (Element Msg)
showDataSummary data =
    let
        entries =
            data.rawEntries |> Dict.size |> String.fromInt

        accounts =
            data.accounts |> Dict.size |> String.fromInt

        categories =
            data.categories |> Dict.size |> String.fromInt
    in
    [ el [] <| text ([ "Database loaded. ", entries, " entries, ", accounts, " accounts, ", categories, " categories." ] |> String.concat)
    , showActions [ LoadOther, Init, Save ]
    ]


showDataIssues : Error -> List (Element Msg)
showDataIssues error =
    [ Notification.showNotification <| Notification.Error [ text "There was an issue loading the data from the DB!" ]
    , el [] <| text <| Decode.errorToString error
    , showActions [ LoadOther, Init ]
    ]


type Action
    = Load
    | LoadOther
    | Init
    | Save


showActions : List Action -> Element Msg
showActions buttons =
    row
        [ width <| minimum 600 fill, Font.size size.m, spacing size.s ]
        (buttons
            |> List.map
                (\button ->
                    case button of
                        Load ->
                            Input.button Layout.style.button
                                { onPress = Just PickFile
                                , label = text "Load a DB json file."
                                }

                        LoadOther ->
                            Input.button Layout.style.button
                                { onPress = Just PickFile
                                , label = text "Load another DB json file"
                                }

                        Init ->
                            Input.button Layout.style.button
                                { onPress = Just InitDatabase
                                , label = text "Initialize an empty DB"
                                }

                        Save ->
                            Input.button Layout.style.button
                                { onPress = Just SaveDataBase
                                , label = text "Save DB"
                                }
                )
        )
