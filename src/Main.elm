module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (..)
import Serialize

main = Browser.sandbox { init = init, update = update, view = view }

type alias Data = List (List String)
type alias UI =
  { row: String
  }

type Model
  = Init String
  | Show Data UI
  | ShowSave String

init = Init ""

type Msg
  = SetFileName String
  | Load
  | Row String
  | AddRow
  | Save

update : Msg -> Model -> Model
update msg model =
  case (model, msg) of
    (Init _, SetFileName f) -> Init f
    (Init f, Load) -> Show (decode f) (UI "")
    (Show d ui, AddRow) -> Show (d ++ [ ui.row |> String.split "," |> List.map String.trim ]) ui
    (Show d ui, Row r) -> Show d { ui | row = r }
    (Show d ui, Save) -> ShowSave (encode d)
    (_, _) -> model

dataCodec = Serialize.list <| Serialize.list Serialize.string
encode data = Serialize.encodeToString dataCodec data
decode data = case (Serialize.decodeFromString dataCodec data) of
  Ok value -> value
  Err (Serialize.CustomError e) -> [["Custom"]]
  Err Serialize.DataCorrupted -> [["corrupt"]]
  Err Serialize.SerializerOutOfDate -> [["outofdate"]]

view : Model -> Html Msg
view model =
  case model of
    Init s -> showInit
    Show data ui ->
      div [] [
        div [ id "menu" ] [ text "Titel" ]
      , div [ id "data" ] [
          div [ id "edit" ] [
            input [ placeholder "Text to add", onInput Row] []
          , button [ onClick AddRow ] [ text "Add" ]
          ]
        , div [ id "ledger" ] ( viewTable data )
        , button [ onClick Save ] [ text "Save" ]
        ]
      ]
    ShowSave d -> text d

viewTable table = table |> List.map (
  \row -> tr [] ( row |> List.map ( \t -> td [] [ text t ]) )
  )

showInit = div [] [
             input [ placeholder "File", onInput SetFileName] []
           , button [ onClick Load ] [ text "Load" ]
           ]
