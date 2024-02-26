module Util.Date exposing (..)

import Time.Date as Date exposing (Date)


compareMonths : Date -> Date -> Order
compareMonths d1 d2 =
    Date.compare (Date.setDay 1 d1) (Date.setDay 1 d2)
