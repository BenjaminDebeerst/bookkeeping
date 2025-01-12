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

import Dict exposing (Dict)
import Effect exposing (Effect)
import Json.Decode exposing (..)
import Persistence.Data as Data exposing (Data)
import Result.Extra
import Route exposing (Route)
import Route.Path as Path
import Shared.Model exposing (AppState, Model(..))
import Shared.Msg exposing (Msg(..))


type alias Flags =
    Maybe Data


type alias Model =
    Shared.Model.Model


type alias Msg =
    Shared.Msg.Msg


decoder : Json.Decode.Decoder Flags
decoder =
    -- While the encoding format is JSON, it is written to JS as
    -- string literal (json withing a json string), for compatibility with storing in a file (which is also a string)
    -- So we first need to unpack the json string, and then run the actual
    -- json parsing on that
    string |> andThen decodeStorageString


decodeStorageString : String -> Decoder Flags
decodeStorageString s =
    if s == "" then
        succeed Nothing

    else
        Json.Decode.decodeString Data.jsonDecoder s
            |> Result.Extra.unpack (errorToString >> fail) succeed
            |> map Just


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init result _ =
    case result of
        Ok (Just data) ->
            ( Loaded data noQuery, Effect.none )

        Ok Nothing ->
            ( None, Effect.none )

        Err error ->
            ( Problem error, Effect.none )


noQuery =
    AppState Nothing Nothing


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Update data ->
            ( Loaded data (queryFrom model), Effect.saveToLocalStorage data )

        TruncateDB ->
            ( Loaded Data.empty noQuery, storeAndForward Data.empty )

        LoadDatabase db ->
            case db |> decodeString Data.jsonDecoder of
                Ok data ->
                    ( Loaded data noQuery, storeAndForward data )

                Err error ->
                    ( Problem error, Effect.none )

        CloseDB ->
            ( None, Effect.batch [ Effect.deleteDB, Effect.pushRoutePath Path.Home_ ] )

        SetDateRange min max ->
            case model of
                Loaded d s ->
                    ( Loaded d { s | min = min, max = max }, Effect.none )

                _ ->
                    ( model, Effect.none )


queryFrom : Model -> AppState
queryFrom model =
    case model of
        Loaded _ query ->
            query

        _ ->
            noQuery


storeAndForward : Data -> Effect Msg
storeAndForward data =
    Effect.batch [ Effect.store data, Effect.pushRoutePath Path.Book ]


dataSummary : Model -> Maybe ( Int, Int, Int )
dataSummary model =
    case model of
        None ->
            Nothing

        Problem _ ->
            Nothing

        Loaded data _ ->
            let
                entries =
                    data.rawEntries.entries |> Dict.size

                accounts =
                    data.accounts |> Dict.size

                categories =
                    data.categories |> Dict.size
            in
            Just ( entries, accounts, categories )
