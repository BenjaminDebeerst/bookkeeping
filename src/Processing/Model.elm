module Processing.Model exposing (..)

import Dict
import Persistence.Data exposing (Account, Data, RawAccountEntry)
import Processing.Csv exposing (Entry, parseEntries)
import Time.Date as Date


getEntries : Data -> List Filter -> Order comparable -> List Entry
getEntries data filters order =
    Dict.values data.rawEntries
        |> parseEntries
        |> List.filter (all filters)
        |> List.sortBy order



-- Filter entries


type alias Filter =
    Entry -> Bool


all : List Filter -> Filter
all l =
    \e -> List.all (\f -> f e) l


filterMonth : Int -> Entry -> Bool
filterMonth i e =
    Date.month e.date == i


filterYear : Int -> Entry -> Bool
filterYear i e =
    Date.year e.date == i


filterDescription : String -> Entry -> Bool
filterDescription s e =
    if String.isEmpty s then
        True

    else
        String.contains (String.toLower s) (String.toLower e.description)



-- Order entries


type alias Order comparable =
    Entry -> comparable


dateAsc =
    \e -> Date.toTuple e.date


dateDesc =
    \e -> negate <| Date.toTuple e.date


negate ( a, b, c ) =
    ( -a, -b, -c )
