module Processing.Csv exposing (parseEntries)

import Array exposing (Array)
import Maybe.Extra
import Persistence.Data exposing (RawAccountEntry)
import Processing.Model exposing (Entry)
import Time.Date as Date exposing (Date)


parseEntries : List RawAccountEntry -> List Entry
parseEntries l =
    l
        |> List.map parseCsvLine
        |> List.map Result.toMaybe
        |> Maybe.Extra.values



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvLine : RawAccountEntry -> Result RawAccountEntry Entry
parseCsvLine raw =
    let
        cells =
            raw.entry.line |> String.split ";" |> List.map String.trim |> Array.fromList

        amount =
            Array.get 11 cells |> Maybe.map (onlyNumberChars >> String.toInt) |> Maybe.Extra.join
    in
    Maybe.map5 Entry
        (Just raw.entry.id)
        (Maybe.Extra.join <| Maybe.map toDate <| Array.get 4 cells)
        (combineTexts [ Array.get 6 cells, Array.get 9 cells, Array.get 10 cells ])
        amount
        (Just raw)
        |> Maybe.map Ok
        |> Maybe.withDefault (Err raw)


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


toDate :
    String
    -> Maybe Date -- TODO error handling
toDate s =
    -- format 25.3.1970
    case s |> String.split "." |> List.map String.toInt of
        (Just a) :: (Just b) :: (Just c) :: [] ->
            Just <| Date.date c b a

        _ ->
            Nothing
