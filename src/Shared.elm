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
import Json.Decode exposing (Error)
import Persistence.Data as Data exposing (Data)
import Result.Extra
import Route exposing (Route)
import Shared.Model exposing (Model(..))
import Shared.Msg exposing (Msg(..))


type alias Flags =
    Model


type alias Model =
    Shared.Model.Model


type alias Msg =
    Shared.Msg.Msg


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map
        (\s ->
            if s == "" then
                None

            else
                Json.Decode.decodeString Data.jsonDecoder s |> toModel
        )
        Json.Decode.string


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init result _ =
    ( Result.Extra.unpack Problem identity result
    , Effect.none
    )


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Update data ->
            ( Loaded data, Effect.saveToLocalStorage data )

        TruncateDB ->
            ( model, Effect.store Data.empty )

        LoadDatabase db ->
            case db |> Json.Decode.decodeString Data.jsonDecoder |> toModel of
                None ->
                    ( model, Effect.store Data.empty )

                Loaded data ->
                    ( model, Effect.store data )

                Problem error ->
                    ( Problem error, Effect.none )


toModel : Result Error Data -> Model
toModel result =
    case result of
        Ok data ->
            Loaded data

        Err e ->
            Problem e
