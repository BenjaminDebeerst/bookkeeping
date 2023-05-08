module Processing.Model exposing (..)

import Persistence.Data exposing (RawAccountEntry)
import Time.Date exposing (Date)


type alias Entry =
    { id : String
    , date : Date
    , description : String
    , amount : Int
    , raw : RawAccountEntry
    }
