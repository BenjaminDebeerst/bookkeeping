module CsvParser exposing (Entry, toEntries)

import Array
import Dict exposing (Dict)
import Maybe.Extra


type alias Entry =
    { id : String
    , date : String
    , description : String
    , amount : Int
    }


toEntries : Dict String String -> List Entry
toEntries l =
    l
        |> Dict.map parseCsvLine
        |> Dict.values
        |> Maybe.Extra.values



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvLine : String -> String -> Maybe Entry
parseCsvLine key line =
    let
        cells =
            line |> String.split ";" |> List.map String.trim |> Array.fromList

        date =
            get cells 4

        descr =
            [ get cells 6, get cells 9, get cells 10 ] |> List.intersperse " " |> String.concat

        r =
            String.replace

        amount =
            String.toInt <| r "," "" <| r "." "" <| r " " "" <| get cells 11
    in
    Maybe.map (\a -> { id = key, date = date, description = descr, amount = a }) amount


get a i =
    Maybe.withDefault "X" <| Array.get i a
