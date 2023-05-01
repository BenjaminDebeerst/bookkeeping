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
    { data : Data }


type Msg
    = StorageUpdated Data


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { data = decode flags }
    , Cmd.none
    )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange StorageUpdated


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated data ->
            ( { model | data = data }
            , Cmd.none
            )
