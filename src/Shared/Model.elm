module Shared.Model exposing (AppState, Model(..))

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}

import Json.Decode exposing (Error)
import Persistence.Data exposing (Data)
import Time.Date exposing (Date)


type Model
    = None
    | Loaded Data AppState
    | Problem Error


type alias AppState =
    { min : Maybe Date
    , max : Maybe Date
    }
