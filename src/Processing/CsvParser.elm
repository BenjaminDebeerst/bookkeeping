module Processing.CsvParser exposing (ParsedRow, parse, parseCsvLine)

import Csv.Decode as Decode exposing (Decoder, Error(..), column, string)
import Csv.Parser as Parser
import Persistence.Data exposing (ImportProfile)
import Time.Date as Date exposing (Date)


type alias ParsedRow =
    { date : Date
    , description : String
    , amount : Int
    , rawLine : String
    }


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


{-| While Csv.Parser.parse is exposed, the internal function to implement Decode.decodeCustom
in terms of Csv.Parser.parse is not (in particular: Decode.applyDecoder). This means we need
to parse the CSV twice in order to lay hands on the raw row _and_ the decoded entities. :-/
-}
parse : ImportProfile -> String -> Result Decode.Error (List ParsedRow)
parse profile s =
    Result.map2
        joinRawLinesAndParsedRows
        (parseIntoRows profile s)
        (Decode.decodeCustom
            { fieldSeparator = profile.splitAt }
            Decode.FieldNamesFromFirstRow
            (rowDecoder profile)
            s
        )


joinRawLinesAndParsedRows : List String -> List ParsedRow -> List ParsedRow
joinRawLinesAndParsedRows =
    List.map2 (\raw parsed -> { parsed | rawLine = raw })


{-| Parse into the raw Strings that make up the rows, dropping the header row
-}
parseIntoRows : ImportProfile -> String -> Result Decode.Error (List String)
parseIntoRows profile s =
    Parser.parse
        { fieldSeparator = profile.splitAt }
        s
        |> Result.mapError ParsingError
        |> Result.map (List.map (joinColumns profile))
        |> Result.map (List.drop 1)


joinColumns : ImportProfile -> List String -> String
joinColumns profile row =
    row
        |> List.map (quoteIfNecessary (String.fromChar profile.splitAt))
        -- TODO: use Csv.Encode instead of String.join?
        |> String.join (String.fromChar profile.splitAt)


rowDecoder : ImportProfile -> Decoder ParsedRow
rowDecoder profile =
    Decode.into ParsedRow
        |> Decode.pipeline (column profile.dateField dateDecoder)
        |> Decode.pipeline (combinedTextColumns profile.descrFields)
        |> Decode.pipeline (column profile.amountField twoDigitFloatToIntDecoder)
        -- Since Csv.Decode provides no way to lay hands on the entire row in a generic way,
        -- the CSV is parsed in raw format separately in order to populate this field.
        |> Decode.pipeline (Decode.succeed "")


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
