module Shared.Msg exposing (Msg(..))

import Persistence.Data exposing (Data)
import Time.Date exposing (Date)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = TruncateDB
    | LoadDatabase String
    | Update Data
    | CloseDB
    | SetDateRange (Maybe Date) (Maybe Date)
