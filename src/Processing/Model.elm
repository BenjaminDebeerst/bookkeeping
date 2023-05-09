module Processing.Model exposing (..)

import Dict
import Persistence.Data exposing (Account, Data, RawAccountEntry)
import Processing.Csv exposing (Entry, parseEntries)
import Time.Date as Date


getEntries : Data -> List Filter -> Ordering Entry -> List Entry
getEntries data filters order =
    Dict.values data.rawEntries
        |> parseEntries
        |> List.filter (all filters)
        |> List.sortWith order



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


type alias Ordering a =
    a -> a -> Basics.Order


asc : (Entry -> comparable) -> Ordering Entry
asc f =
    \e1 e2 -> compare (f e1) (f e2)


desc : (Entry -> comparable) -> Ordering Entry
desc f =
    \e1 e2 -> compare (f e2) (f e1)


dateAsc =
    asc (\e -> Date.toTuple e.date)


dateDesc =
    desc (\e -> Date.toTuple e.date)
