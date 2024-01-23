module Shared exposing
    ( Flags
    , Model
    , Msg
    , decoder
    , init
    , subscriptions
    , update
    )

import Effect exposing (Effect)
import Json.Decode
import Persistence.Data exposing (Data, decode)
import Route exposing (Route)
import Serialize exposing (Error)
import Shared.Model exposing (Model(..))
import Shared.Msg exposing (Msg(..))


type alias Flags =
    String


type alias Model =
    Shared.Model.Model


type alias Msg =
    Shared.Msg.Msg


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.string


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init result _ =
    ( result |> Result.withDefault "" |> decode |> toModel
    , Effect.none
    )


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg _ =
    case msg of
        TruncateDB ->
            ( None, Effect.none )

        LoadDatabase db ->
            ( db |> decode |> toModel, Effect.none )

        Update data ->
            ( Loaded data, Effect.none )


toModel : Result (Error String) (Maybe Data) -> Model
toModel result =
    case result of
        Ok Nothing ->
            None

        Ok (Just data) ->
            Loaded data

        Err e ->
            Problem e
