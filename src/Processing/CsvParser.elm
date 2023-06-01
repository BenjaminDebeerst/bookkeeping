module Processing.CsvParser exposing (ParsedRow, csvRows, parse, parseCsvLine)

import Csv.Decode as Decode exposing (Decoder, Error(..), column, string)
import Persistence.Data exposing (ImportProfile)
import Time.Date as Date exposing (Date)


type alias ParsedRow =
    { date : Date
    , description : String
    , amount : Int
    }


csvRows : String -> Result Decode.Error (List String)
csvRows csvFile =
    Decode.decodeCustom
        { fieldSeparator = ';' }
        Decode.FieldNamesFromFirstRow
        -- mock implementation, only works for exactly 12 columns
        ([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
            |> List.map (\i -> column i string)
            |> traverse
            |> Decode.map (List.map (quoteIfNecessary ";"))
            |> Decode.map (String.join ";")
        )
        csvFile


quoteIfNecessary : String -> String -> String
quoteIfNecessary fieldSeparator value =
    if
        String.contains "\"" value
            || String.contains fieldSeparator value
            || String.contains "\u{000D}\n" value
            || String.contains "\n" value
    then
        "\"" ++ String.replace "\"" "\"\"" value ++ "\""

    else
        value


parse : ImportProfile -> String -> Result Decode.Error (List ParsedRow)
parse profile s =
    Decode.decodeCustom
        { fieldSeparator = profile.splitAt }
        Decode.FieldNamesFromFirstRow
        (rowDecoder profile)
        s


rowDecoder : ImportProfile -> Decoder ParsedRow
rowDecoder profile =
    Decode.into ParsedRow
        |> Decode.pipeline (column profile.dateField dateDecoder)
        |> Decode.pipeline (combinedTextColumns profile.descrFields)
        |> Decode.pipeline (column profile.amountField twoDigitFloatToIntDecoder)


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


parseCsvLine : ImportProfile -> String -> Result String ParsedRow
parseCsvLine importProfile line =
    let
        res =
            Decode.decodeCustom
                { fieldSeparator = importProfile.splitAt }
                Decode.NoFieldNames
                (rowDecoder importProfile)
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


toDate : String -> Maybe Date
toDate s =
    -- TODO error handling
    -- format 25.3.1970
    case s |> String.split "." |> List.map String.toInt of
        (Just a) :: (Just b) :: (Just c) :: [] ->
            Just <| Date.date c b a

        _ ->
            Nothing
