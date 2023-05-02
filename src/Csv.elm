module Csv exposing (Unparsed, parseEntries, parseValidEntries)

import Array exposing (Array)
import Maybe.Extra
import Persistence.Data exposing (Entry)
import SHA1


parseValidEntries : List String -> List Entry
parseValidEntries l =
    l
        |> List.map parseCsvLine
        |> List.map Result.toMaybe
        |> Maybe.Extra.values


type alias Unparsed =
    { id : String, text : String }


parseEntries : List String -> List (Result Unparsed Entry)
parseEntries l =
    List.map parseCsvLine l


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvLine : String -> Result Unparsed Entry
parseCsvLine line =
    let
        id =
            sha1 line

        cells =
            line |> String.split ";" |> List.map String.trim |> Array.fromList

        amount =
            Array.get 11 cells |> Maybe.map (onlyNumberChars >> String.toInt) |> Maybe.Extra.join
    in
    Maybe.map4 Entry
        (Just id)
        (Array.get 4 cells)
        (combineTexts [ Array.get 6 cells, Array.get 9 cells, Array.get 10 cells ])
        amount
        |> Maybe.map Ok
        |> Maybe.withDefault (Unparsed id line |> Err)


{-| If all items are Justs, shorten inner whitespace, combine them with \\n and trim the result
-}
combineTexts : List (Maybe String) -> Maybe String
combineTexts list =
    list
        |> List.map (Maybe.map deduplicateSpaces)
        |> List.foldr (Maybe.map2 (\s l -> s :: l)) (Just [])
        |> Maybe.map (List.intersperse "\n")
        |> Maybe.map (String.concat >> String.trim)


deduplicateSpaces : String -> String
deduplicateSpaces s =
    s |> String.split " " |> List.filter (not << String.isEmpty) |> String.join " "


onlyNumberChars : String -> String
onlyNumberChars s =
    String.filter (\c -> Char.isDigit c || c == '-') s
