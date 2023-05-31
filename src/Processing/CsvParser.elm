module Processing.CsvParser exposing (ParsedRow, parse, parseCsvLine)

import Csv.Decode as Decode exposing (Decoder, Error(..), column, string)
import Time.Date as Date exposing (Date)


type alias ParsedRow =
    { date : Date
    , description : String
    , amount : Int
    }


parse : String -> Result Decode.Error (List ParsedRow)
parse s =
    Decode.decodeCustom
        { fieldSeparator = ';' }
        Decode.FieldNamesFromFirstRow
        rowDecoder
        s


rowDecoder : Decoder ParsedRow
rowDecoder =
    Decode.into ParsedRow
        |> Decode.pipeline (column 4 dateDecoder)
        |> Decode.pipeline (combinedTextColumns [ 6, 9, 10 ])
        |> Decode.pipeline (column 11 twoDigitFloatToIntDecoder)


dateDecoder : Decoder Date
dateDecoder =
    contextualDecoder
        toDate
        (String.append "No date of format dd.mm.yyyy: ")


twoDigitFloatToIntDecoder : Decoder Int
twoDigitFloatToIntDecoder =
    contextualDecoder
        (onlyNumberChars >> String.toInt)
        (String.append "Not a number: ")


combinedTextColumns : List Int -> Decoder String
combinedTextColumns columns =
    columns
        |> List.map (\i -> column i string)
        |> traverse
        |> Decode.map sanitizeTextFields
        |> Decode.map (String.join "\n")


sanitizeTextFields : List String -> List String
sanitizeTextFields =
    List.map String.trim
        >> List.filter (not << String.isEmpty)
        >> List.map deduplicateSpaces


traverse : List (Decoder a) -> Decoder (List a)
traverse decoders =
    case decoders of
        [] ->
            Decode.succeed []

        head :: tail ->
            Decode.map2
                (\h t -> h :: t)
                head
                (traverse tail)


contextualDecoder : (String -> Maybe b) -> (String -> String) -> Decoder b
contextualDecoder decoder error =
    string
        |> Decode.map (\s -> ( decoder s, s ))
        |> Decode.andThen (\( decoded, s ) -> Decode.fromMaybe (error s) decoded)



-- Arguments to generalize over later:
-- The split char ';'
-- The date column 4
-- The descr column(s)
-- The amount column


parseCsvLine : String -> Result String ParsedRow
parseCsvLine line =
    let
        res =
            Decode.decodeCustom
                { fieldSeparator = ';' }
                Decode.NoFieldNames
                rowDecoder
                line
    in
    res
        |> Result.mapError Decode.errorToString
        |> Result.andThen
            (List.head
                >> Maybe.map Ok
                >> Maybe.withDefault (Err <| "Single row was empty o_O: " ++ line)
            )


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
