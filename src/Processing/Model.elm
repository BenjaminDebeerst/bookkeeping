module Processing.Model exposing (..)

import Persistence.Data exposing (Account, RawAccountEntry)
import SHA1
import Time.Date exposing (Date)


type alias Entry =
    { id : String
    , date : Date
    , description : String
    , amount : Int
    }
