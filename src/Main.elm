module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (..)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { content : List (List String)
  , newContent : String
  }

init : Model
init =
  { content = []
  , newContent = ""
  }

type Msg =
    Draft String
  | Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    Update ->
      { model | content = model.content ++ [ model.newContent |> String.split "," |> List.map String.trim ] }
    Draft newContent -> { model | newContent = newContent }

view : Model -> Html Msg
view model =
  div [] [
    div [ id "menu" ] [ text "Titel" ]
  , div [ id "content" ] [
      div [ id "edit" ] [
        input [ placeholder "Text to add", onInput Draft] []
      , button [ onClick Update ] [ text "Add" ]
      ]
    , div [ id "ledger" ] ( viewTable model.content )
    ]
  ]

viewTable table = table |> List.map (
  \row -> tr [] ( row |> List.map ( \t -> td [] [ text t ]) )
  )
