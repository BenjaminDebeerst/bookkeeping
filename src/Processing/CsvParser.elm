module Processing.CsvParser exposing (ParsedRow, errorToString, parse, parseCsvLine, toDate)

import Csv.Decode as Decode exposing (Decoder, Error(..), column, string)
import Csv.Parser as Parser
import Persistence.ImportProfile exposing (DateFormat(..), ImportProfile)
import Time.Date as Date exposing (Date)


type alias ParsedRow =
    { date : Date
    , description : String
    , amount : Int
    , category : Maybe String
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
        |> Decode.pipeline (column profile.dateField <| dateDecoder profile.dateFormat)
        |> Decode.pipeline (combinedTextColumns profile.descrFields)
        |> Decode.pipeline (column profile.amountField twoDigitFloatToIntDecoder)
        |> Decode.pipeline (maybeColumn profile.categoryField)
        -- Since Csv.Decode provides no way to lay hands on the entire row in a generic way,
        -- the CSV is parsed in raw format separately in order to populate this field.
        |> Decode.pipeline (Decode.succeed "")


maybeColumn : Maybe Int -> Decoder (Maybe String)
maybeColumn column =
    case column of
        Nothing ->
            Decode.succeed Nothing

        Just i ->
            Decode.column i Decode.string
                |> Decode.map
                    (\s ->
                        if String.trim s == "" then
                            Nothing

                        else
                            Just s
                    )


dateDecoder : DateFormat -> Decoder Date
dateDecoder df =
    Decode.string
        |> Decode.andThen (Decode.fromResult << toDate df)


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


toDate : DateFormat -> String -> Result String Date
toDate df s =
    case df of
        YYYYMMDD char ->
            toDateHelper (String.fromChar char) False s

        DDMMYYYY char ->
            toDateHelper (String.fromChar char) True s


toDateHelper : String -> Bool -> String -> Result String Date
toDateHelper split reverse string =
    let
        parts =
            string |> String.split split |> List.map String.toInt

        ( yearMonthDay, format ) =
            if reverse then
                ( List.reverse parts, [ "DD", "MM", "YYYY" ] )

            else
                ( parts, [ "YYYY", "MM", "DD" ] )
    in
    case yearMonthDay of
        (Just year) :: (Just month) :: (Just day) :: [] ->
            Ok <| Date.date year month day

        _ ->
            Err <| "Date not of format " ++ String.join split format ++ ": " ++ string


errorToString : Error -> String
errorToString error =
    case error of
        Decode.DecodingErrors list ->
            let
                n =
                    List.length list

                prefix =
                    String.join "" [ "There were ", String.fromInt n, " errors parsing the CSV. Showing the first few: " ]

                errs =
                    list |> List.take 10 |> List.map (\e -> Decode.DecodingErrors [ e ]) |> List.map Decode.errorToString
            in
            prefix :: errs |> String.join "\n"

        other ->
            other |> Decode.errorToString
