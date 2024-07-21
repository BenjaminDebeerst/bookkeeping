module Components.CsvView exposing (Headers, parse, view, viewWithCustomHeaders)

import Components.Table as T
import Csv.Parser as Parser exposing (Problem(..))
import Element exposing (Element, clipY, el, fill, height, indexedTable, scrollbarX, text, width)
import List.Extra


type alias Csv =
    Result Problem (List (List String))


parse : Maybe Char -> String -> Csv
parse separator content =
    case separator of
        Nothing ->
            content |> String.trim |> String.split "\n" |> List.map (\row -> [ row ]) |> Ok

        Just c ->
            Parser.parse { fieldSeparator = c } content


view : Csv -> Element msg
view csv =
    viewWithCustomHeaders csv (\_ -> Nothing)


viewWithCustomHeaders : Csv -> Headers msg -> Element msg
viewWithCustomHeaders csv headers =
    case csv of
        Ok rows ->
            previewTable rows headers

        Err problem ->
            showCsvProblem problem


{-| For a given column index and header title, provide a custom header cell
-}
type alias Headers msg =
    ( Int, String ) -> Maybe (Element msg)


previewTable : List (List String) -> Headers msg -> Element msg
previewTable csv customHeaders =
    let
        headers =
            csv |> List.head |> Maybe.withDefault []

        header i title =
            customHeaders ( i, title ) |> Maybe.withDefault (text title)

        column =
            \i title -> T.fullStyledColumn (header i title) (cell i)

        cell =
            \i -> List.Extra.getAt i >> Maybe.withDefault "n/a" >> text
    in
    -- workaround: it's necessary to wrap `indexedTable` in an `el` to get table width attribute to apply when using scrollbarX
    el [ width fill, height fill ] <|
        indexedTable T.style.fullWidthTable
            { data = csv |> List.drop 1
            , columns = headers |> List.indexedMap column
            }


showCsvProblem : Problem -> Element msg
showCsvProblem problem =
    case problem of
        SourceEndedWithoutClosingQuote _ ->
            text "Problem parsing CSV file: Unclosed quoted text field."

        AdditionalCharactersAfterClosingQuote _ ->
            text "Problem parsing CSV file: Unexpected text after quoted string before field separator."
