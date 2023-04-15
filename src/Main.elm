module Main exposing (..)

import Array
import Browser
import Browser.Navigation as Nav exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (..)
import Serialize as S
import SHA1
import Url exposing (Url)
import Url.Builder

main = Browser.application
    { init = init
    , onUrlChange = \_ -> Noop
    , onUrlRequest = \_ -> Noop
    , subscriptions = \_ -> Sub.none
    , update = update
    , view = \m -> {title = "App", body = [view m] }
    }

type alias CsvLine = (String, String)

type alias Data =
    { rawData : List CsvLine
    }
type alias UI =
  { textinput: String
  }

type Model
  = Init String Nav.Key
  | Show Data UI Nav.Key
  | ShowSave String

--init : flags -> Url -> Key -> (Cmd msg)
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init f url key = case url.query of
    Nothing -> (Init "" key, Cmd.none)
    -- gonna be lazy here, assuming the query is either data=foobar or absent
    Just data ->
        if String.startsWith "data=" data then
            (Show (decode <| String.dropLeft 5 data) (UI "") key, Cmd.none)
        else
            (Init "" key, Cmd.none)

type Msg
  -- Initialization
  = SetEncodedData String
  | Load
  | FromScratch
  -- CSV Import
  | DataInputModified String
  | StoreData
  | Save
  -- Just for the application constructor
  | Noop

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case (model, msg) of
    (Init _ k, SetEncodedData f) -> (Init f k, Cmd.none)
    (Init f k, Load) -> (Show (decode f) (UI "") k, Cmd.none)
    (Init _ k, FromScratch) -> (Show emptyData (UI "") k, Cmd.none)
    (Show d ui k, StoreData) -> (Show (addCsvRows d ui.textinput ) { ui | textinput = "" } k, Cmd.none)
    (Show d ui k, DataInputModified r) -> (Show d { ui | textinput = r } k, Cmd.none)
    (Show d _ k, Save) -> (model, pushUrl k <| toUrl d)
    (_, _) -> (model, Cmd.none)

toUrl : Data -> String
toUrl data = Url.Builder.relative [] [ Url.Builder.string "data" (encode data) ]

addCsvRows : Data -> String -> Data
addCsvRows data newRows =
    let
      newData = textInputToTable newRows
    in
      { data | rawData = data.rawData ++ newData }

textInputToTable : String -> List CsvLine
textInputToTable data = data |> String.split "\n" |> List.map (\l -> (sha1 l, l))

dataCodec = S.record Data
    |> S.field .rawData (S.list <| S.tuple S.string S.string)
    |> S.finishRecord
encode data = S.encodeToString dataCodec data
decode data = case (S.decodeFromString dataCodec (String.trim data)) of
  Ok value -> value
  Err (_) -> emptyData

emptyData = { rawData = [] }

view : Model -> Html Msg
view model =
  case model of
    Init i _ -> showInit i
    Show data _ _ -> showData data
    ShowSave d -> text d

sha1 : String -> String
sha1 s = SHA1.fromString s |> SHA1.toHex |> String.slice 0 8

showInit i = div [] [
             div [] [ textarea [ placeholder "Serialized data to load", onInput SetEncodedData] [] ]
           , div [] [ button [ onClick Load ] [ text "Load" ] ]
           , div [] [ button [ onClick FromScratch ] [ text "Start from Scratch" ] ]
           ]

showData : Data -> Html Msg
showData data =
    let
      table = tableFromCsvData data.rawData
    in
    div [] [
      div [ id "menu" ] [ ]
    , div [ id "data" ] [
        div [ id "edit" ] [
          div [] [ textarea [ placeholder "Import CSV data", onInput DataInputModified] [] ]
        , div [] [ button [ onClick StoreData ] [ text "Add" ] ]
        ]
      , div [ id "ledger" ] ( viewTable table )
      , button [ onClick Save ] [ text "Save" ]
      ]
    ]

tableFromCsvData : List CsvLine -> List (List String)
tableFromCsvData l = l |> List.map (\tuple -> [ (Tuple.first tuple) ] ++ parseCsvRow (Tuple.second tuple))

-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column
parseCsvRow s =
    let
        cells = s |> String.split ";" |> List.map String.trim |> Array.fromList
        date = get cells 4
        descr = [get cells 6, get cells 9, get cells 10] |> List.intersperse " " |> String.concat
        amount = get cells 11
    in
      [  date, descr, amount ]

get a i = Maybe.withDefault "X" <| Array.get i a

viewTable : List (List String) -> List (Html Msg)
viewTable t = t |> List.map ( \r -> tr [] (r |> List.map (\c -> td [] [ text c ])))