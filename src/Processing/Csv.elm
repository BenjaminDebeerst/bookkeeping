module Processing.Csv exposing (Entry, parseEntries)

import Array exposing (Array)
import Dict exposing (Dict)
import Maybe.Extra
import Persistence.Data exposing (Account, RawAccountEntry)
import Time.Date as Date exposing (Date)


type alias Entry =
    { id : String
    , date : Date
    , description : String
    , amount : Int
    , account : Account
    , raw : RawAccountEntry
    }


parseEntries : Dict Int Account -> List RawAccountEntry -> List Entry
parseEntries accounts rawEntries =
    rawEntries
        |> List.map (parseCsvLine accounts)
        |> List.map Result.toMaybe
        |> Maybe.Extra.values



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvLine : Dict Int Account -> RawAccountEntry -> Result RawAccountEntry Entry
parseCsvLine accounts raw =
    let
        andMap =
            Maybe.map2 (|>)

        cells =
            raw.entry.line |> String.split ";" |> List.map String.trim |> Array.fromList

        amount =
            Array.get 11 cells |> Maybe.map (onlyNumberChars >> String.toInt) |> Maybe.Extra.join

        account =
            Dict.get raw.account accounts
    in
    Just Entry
        |> andMap (Just raw.entry.id)
        |> andMap (Maybe.Extra.join <| Maybe.map toDate <| Array.get 4 cells)
        |> andMap (combineTexts [ Array.get 6 cells, Array.get 9 cells, Array.get 10 cells ])
        |> andMap amount
        |> andMap account
        |> andMap (Just raw)
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
