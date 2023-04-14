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
  { textinput: String
  }

type Model
  = Init String
  | Show Data UI
  | ShowSave String

init = Init ""

type Msg
  -- Initialization
  = SetEncodedData String
  | Load
  | FromScratch
  -- CSV Import
  | DataInputModified String
  | StoreData
  | Save

update : Msg -> Model -> Model
update msg model =
  case (model, msg) of
    (Init _, SetEncodedData f) -> Init f
    (Init f, Load) -> Show (decode f) (UI "")
    (Init _, FromScratch) -> Show [] (UI "")
    (Show d ui, StoreData) -> Show (d ++  (textInputToTable ui.textinput) ) ui
    (Show d ui, DataInputModified r) -> Show d { ui | textinput = r }
    (Show d _, Save) -> ShowSave (encode d)
    (_, _) -> model

textInputToTable data = data |> String.split "\n" |> List.map splitAtComma
splitAtComma s = s |> String.split "," |> List.map String.trim

dataCodec = Serialize.list <| Serialize.list Serialize.string
encode data = Serialize.encodeToString dataCodec data
decode data = case (Serialize.decodeFromString dataCodec (String.trim data)) of
  Ok value -> value
  Err (_) -> [["I'm sorry, something went wrong"]]

view : Model -> Html Msg
view model =
  case model of
    Init _ -> showInit
    Show data _ ->
      div [] [
        div [ id "menu" ] [ text "Titel" ]
      , div [ id "data" ] [
          div [ id "edit" ] [
            div [] [ textarea [ placeholder "Import CSV data", onInput DataInputModified] [] ]
          , div [] [ button [ onClick StoreData ] [ text "Add" ] ]
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
             div [] [ textarea [ placeholder "Data to be loaded", onInput SetEncodedData] [] ]
           , div [] [ button [ onClick Load ] [ text "Load" ] ]
           , div [] [ button [ onClick FromScratch ] [ text "Start from Scratch" ] ]
           ]
