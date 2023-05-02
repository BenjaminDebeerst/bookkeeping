module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Persistence.Data exposing (Data, decode)
import Persistence.Storage as Storage
import Request exposing (Request)


type alias Flags =
    String


type alias Model =
    Data


type alias Msg =
    Data


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( decode flags
    , Cmd.none
    )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange identity


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ data _ =
    ( data, Cmd.none )
