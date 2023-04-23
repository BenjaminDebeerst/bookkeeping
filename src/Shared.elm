module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Request exposing (Request)
import Storage exposing (Storage)


type alias Flags =
    String


type alias Model =
    { storage : Storage }


type Msg
    = StorageUpdated Storage


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { storage = Storage.decode flags }
    , Cmd.none
    )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange StorageUpdated


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage ->
            ( { model | storage = storage }
            , Cmd.none
            )
