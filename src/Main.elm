module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { content : List String
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
      { model | content = model.content ++ [ model.newContent ] }
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
    , div [ id "ledger" ] [
        ul [] ( model.content |> List.map viewRow)
      ]
    ]
  ]

viewContent : List String -> List (Html Msg)
viewContent l = List.map viewRow l

viewRow : String -> Html Msg
viewRow r = li [] [ text r ]
