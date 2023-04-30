module Csv exposing (Entry, Unparsed, allEntries, validEntries)

import Array exposing (Array)
import Dict exposing (Dict)
import Maybe.Extra as MaybeE


type alias Entry =
    { id : String
    , date : String
    , description : String
    , amount : Int
    }


validEntries : Dict String String -> List Entry
validEntries l =
    l
        |> Dict.map Tuple.pair
        |> Dict.values
        |> List.map parseCsvLine
        |> List.map Result.toMaybe
        |> MaybeE.values


type alias Unparsed =
    { id : String, text : String }


allEntries : List ( String, String ) -> List (Result Unparsed Entry)
allEntries l =
    List.map parseCsvLine l



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvLine : ( String, String ) -> Result Unparsed Entry
parseCsvLine ( key, line ) =
    let
        cells =
            line |> String.split ";" |> List.map String.trim |> Array.fromList

        amount =
            Array.get 11 cells |> Maybe.map (onlyNumberChars >> String.toInt) |> MaybeE.join
    in
    Maybe.map4 Entry
        (Just key)
        (Array.get 4 cells)
        (combineTexts [ Array.get 6 cells, Array.get 9 cells, Array.get 10 cells ])
        amount
        |> Maybe.map Ok
        |> Maybe.withDefault (Unparsed key line |> Err)


{-| If all items are Justs, combine them with \\n and trim the result
-}
combineTexts : List (Maybe String) -> Maybe String
combineTexts list =
    List.foldr (Maybe.map2 (\s l -> s :: l)) (Just []) list
        |> Maybe.map (List.intersperse "\n")
        |> Maybe.map (String.concat >> String.trim)


onlyNumberChars : String -> String
onlyNumberChars s =
    String.filter (\c -> Char.isDigit c || c == '-') s
