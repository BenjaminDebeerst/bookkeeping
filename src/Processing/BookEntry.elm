module Processing.BookEntry exposing (..)

import Persistence.Data exposing (Account)
import Time.Date exposing (Date)


type alias BookEntry =
    { id : String
    , date : Date
    , description : String
    , amount : Int
    , account : Account
    }
