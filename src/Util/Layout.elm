module Util.Layout exposing
    ( dataUpdate
    , dataView
    )

import Effect exposing (Effect)
import Element exposing (..)
import Persistence.Data exposing (Data)
import Route exposing (Route)
import Route.Path
import Shared exposing (Model)
import Shared.Model exposing (Model(..))
import View exposing (View)


dataView : Shared.Model -> String -> (Data -> pageModel -> Element msg) -> pageModel -> View msg
dataView shared pageTitle content model =
    { title = pageTitle
    , body =
        case shared of
            None ->
                Element.text "There is no data loaded."

            Problem _ ->
                Element.text "Error"

            Loaded data ->
                content data model
    }


type alias UpdateFn msg model =
    msg -> model -> ( model, Effect msg )


dataUpdate : Shared.Model -> (Data -> UpdateFn msg model) -> UpdateFn msg model
dataUpdate shared update =
    case shared of
        None ->
            \_ model -> ( model, Effect.pushRoutePath Route.Path.Home_ )

        Problem _ ->
            \_ model -> ( model, Effect.pushRoutePath Route.Path.Home_ )

        Loaded data ->
            update data
