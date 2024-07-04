module Shared exposing
    ( Flags
    , Model
    , Msg
    , dataSummary
    , decoder
    , init
    , subscriptions
    , update
    )

import Dict
import Effect exposing (Effect)
import Json.Decode exposing (Error)
import Persistence.Data as Data exposing (Data)
import Result.Extra
import Route exposing (Route)
import Route.Path as Path
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
            ( model, storeAndForward Data.empty )

        LoadDatabase db ->
            case db |> Json.Decode.decodeString Data.jsonDecoder |> toModel of
                None ->
                    ( model, storeAndForward Data.empty )

                Loaded data ->
                    ( model, storeAndForward data )

                Problem error ->
                    ( Problem error, Effect.none )

        CloseDB ->
            ( None, Effect.batch [ Effect.deleteDB, Effect.pushRoutePath Path.Home_ ] )


storeAndForward : Data -> Effect Msg
storeAndForward data =
    Effect.batch [ Effect.store data, Effect.pushRoutePath Path.Book ]


toModel : Result Error Data -> Model
toModel result =
    case result of
        Ok data ->
            Loaded data

        Err e ->
            Problem e


dataSummary : Model -> Maybe ( Int, Int, Int )
dataSummary model =
    case model of
        None ->
            Nothing

        Problem _ ->
            Nothing

        Loaded data ->
            let
                entries =
                    data.rawEntries.entries |> Dict.size

                accounts =
                    data.accounts |> Dict.size

                categories =
                    data.categories |> Dict.size
            in
            Just ( entries, accounts, categories )
