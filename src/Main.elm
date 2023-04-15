module Main exposing (..)

import Array
import Browser
import Browser.Navigation as Nav exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (..)
import SHA1
import Serialize as S
import Url exposing (Url)
import Url.Builder


main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = \m -> { title = "App", body = [ view m ] }
        }


type alias CsvLine =
    ( String, String )


type alias Data =
    { rawData : List CsvLine
    }


type alias UI =
    { textinput : String
    }


type Model
    = Init
        { data : String
        , key : Nav.Key
        }
    | Show
        { key : Nav.Key
        , textinput : String
        , data : Data
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init f url key =
    case url.query of
        Nothing ->
            ( Init { data = "", key = key }, Cmd.none )

        -- gonna be lazy here, assuming the query is either data=foobar or absent
        Just data ->
            if String.startsWith "data=" data then
                ( Show { data = decode <| String.dropLeft 5 data, textinput = "", key = key }, Cmd.none )

            else
                ( Init { data = "", key = key }, Cmd.none )


type
    Msg
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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( model, msg ) of
        ( Init i, SetEncodedData newData ) ->
            ( Init { i | data = newData }, Cmd.none )

        ( Init i, Load ) ->
            ( Show { data = decode i.data, textinput = "", key = i.key }, Cmd.none )

        ( Init i, FromScratch ) ->
            ( Show { data = emptyData, textinput = "", key = i.key }, Cmd.none )

        ( Show s, StoreData ) ->
            ( Show { s | data = addCsvRows s.data s.textinput, textinput = "" }, Cmd.none )

        ( Show s, DataInputModified r ) ->
            ( Show { s | textinput = r }, Cmd.none )

        ( Show s, Save ) ->
            ( model, pushUrl s.key <| toUrl s.data )

        ( _, _ ) ->
            ( model, Cmd.none )


toUrl : Data -> String
toUrl data =
    Url.Builder.relative [] [ Url.Builder.string "data" (encode data) ]


addCsvRows : Data -> String -> Data
addCsvRows data newRows =
    { data | rawData = data.rawData ++ textInputToTable newRows }


textInputToTable : String -> List CsvLine
textInputToTable data =
    data |> String.split "\n" |> List.map (\l -> ( sha1 l, l ))


dataCodec =
    S.record Data
        |> S.field .rawData (S.list <| S.tuple S.string S.string)
        |> S.finishRecord


encode data =
    S.encodeToString dataCodec data


decode data =
    case S.decodeFromString dataCodec (String.trim data) of
        Ok value ->
            value

        Err _ ->
            emptyData


emptyData =
    { rawData = [] }


view : Model -> Html Msg
view model =
    case model of
        Init _ ->
            showInit

        Show s ->
            showData s.data


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex |> String.slice 0 8


showInit =
    div []
        [ div [] [ textarea [ placeholder "Serialized data to load", onInput SetEncodedData ] [] ]
        , div [] [ button [ onClick Load ] [ text "Load" ] ]
        , div [] [ button [ onClick FromScratch ] [ text "Start from Scratch" ] ]
        ]


showData : Data -> Html Msg
showData data =
    div []
        [ div [ id "menu" ] []
        , div [ id "data" ]
            [ div [ id "edit" ]
                [ div [] [ textarea [ placeholder "Import CSV data", onInput DataInputModified ] [] ]
                , div [] [ button [ onClick StoreData ] [ text "Add" ] ]
                ]
            , div [ id "ledger" ] (viewTable <| tableFromCsvData data.rawData)
            , button [ onClick Save ] [ text "Save" ]
            ]
        ]


tableFromCsvData : List CsvLine -> List (List String)
tableFromCsvData l =
    l |> List.map (\tuple -> [ Tuple.first tuple ] ++ parseCsvRow (Tuple.second tuple))



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvRow s =
    let
        cells =
            s |> String.split ";" |> List.map String.trim |> Array.fromList

        date =
            get cells 4

        descr =
            [ get cells 6, get cells 9, get cells 10 ] |> List.intersperse " " |> String.concat

        amount =
            get cells 11
    in
    [ date, descr, amount ]


get a i =
    Maybe.withDefault "X" <| Array.get i a


viewTable : List (List String) -> List (Html Msg)
viewTable t =
    t |> List.map (\r -> tr [] (r |> List.map (\c -> td [] [ text c ])))
