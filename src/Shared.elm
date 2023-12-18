module Shared exposing
    ( Flags
    , Model(..)
    , Msg
    , init
    , justData
    , subscriptions
    , update
    )

import Persistence.Data exposing (Data, decode)
import Persistence.Storage as Storage
import Request exposing (Request)
import Serialize exposing (Error)


type alias Flags =
    String


type Model
    = None
    | Loaded Data
    | Problem (Error String)


type alias Msg =
    Model


justData : Model -> Maybe Data
justData model =
    case model of
        None ->
            Nothing

        Loaded data ->
            Just data

        Problem _ ->
            Nothing


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( decode flags |> toModel
    , Cmd.none
    )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange toModel


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ data _ =
    ( data, Cmd.none )


toModel : Result (Error String) (Maybe Data) -> Model
toModel result =
    case result of
        Ok Nothing ->
            None

        Ok (Just data) ->
            Loaded data

        Err e ->
            Problem e
