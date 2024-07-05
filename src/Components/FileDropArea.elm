module Components.FileDropArea exposing (Model, Msg, init, update, view)

import Components.Icons as Icons
import Components.Input exposing (largeButton)
import Config exposing (color, size)
import Effect exposing (Effect)
import Element exposing (Attribute, Element, centerX, centerY, el, fill, height, width)
import Element.Border as Border
import File exposing (File)
import File.Select as Select
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Task


type Model msg
    = Model
        { hover : Bool
        , lift : Msg -> msg
        , file : String -> String -> msg
        }


init : (Msg -> msg) -> (String -> String -> msg) -> Model msg
init lift file =
    Model { hover = False, lift = lift, file = file }


type Msg
    = PickFile
    | DragEnter
    | DragLeave
    | GotFileName File


update : Msg -> Model msg -> ( Model msg, Effect msg )
update msg (Model model) =
    case msg of
        DragEnter ->
            ( Model { model | hover = True }, Effect.none )

        DragLeave ->
            ( Model { model | hover = False }, Effect.none )

        PickFile ->
            ( Model model, Select.file [ "*" ] GotFileName |> Effect.sendCmd |> Effect.map model.lift )

        GotFileName file ->
            ( Model model, Task.perform (model.file (File.name file)) (File.toString file) |> Effect.sendCmd )


view : Model msg -> Element msg
view (Model model) =
    el
        [ width fill
        , height fill
        , Border.dashed
        , Border.color <|
            if model.hover then
                color.brightAccent

            else
                color.darkAccent
        , Border.width size.xs
        , Border.rounded size.xl
        , onEvent "dragenter" (D.succeed DragEnter)
        , onEvent "dragover" (D.succeed DragEnter)
        , onEvent "dragleave" (D.succeed DragLeave)
        , onEvent "drop" (fileDropDecoder GotFileName)
        ]
        (el [ centerX, centerY ] <| largeButton PickFile Icons.folder "Select File")
        |> Element.map model.lift


fileDropDecoder : (File -> msg) -> D.Decoder msg
fileDropDecoder msg =
    D.map msg (D.at [ "dataTransfer", "files" ] (D.oneOrMore (\a -> \_ -> a) File.decoder))


onEvent : String -> D.Decoder msg -> Attribute msg
onEvent event decoder =
    preventDefaultOn event (D.map (\msg -> ( msg, True )) decoder) |> Element.htmlAttribute
