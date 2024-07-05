module Processing.Annotations exposing (AnnotatedRow, AnnotatedRows, Categorization(..), annotate, categoryCell)

-- ANNOTATIONS

import Components.Icons exposing (checkMark, folderPlus, infoMark, warnTriangle)
import Components.RangeSlider as RangeSlider
import Components.Tooltip exposing (tooltip)
import Config exposing (color, size)
import Dict
import Element exposing (Attribute, Color, Element, el, fill, onRight, row, text, width)
import Element.Font as Font
import List.Extra
import Maybe.Extra
import Parser
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data)
import Processing.CategorizationRules exposing (applyAllCategorizationRules)
import Processing.CategoryParser as CategoryParser
import Processing.CsvParser exposing (ParsedRow)
import Processing.Filter exposing (filterParsedRowDateRange)
import Processing.Model exposing (getCategoryByShort)
import Time.Date exposing (Date)


type alias AnnotatedRow =
    { parsedRow : ParsedRow
    , category : Maybe Categorization
    }


type alias AnnotatedRows =
    { csvSize : Int
    , nExcluded : Int
    , newCategories : List String
    , filteredRows : List AnnotatedRow
    }


annotate : Data -> RangeSlider.Model Date -> List ParsedRow -> AnnotatedRows
annotate data filter rows =
    let
        csvSize =
            List.length rows

        filteredRows =
            rows |> List.filter (filterParsedRowDateRange (RangeSlider.min filter) (RangeSlider.max filter))

        --rows
        nExcluded =
            List.length rows - List.length filteredRows

        annotatedRows =
            annotateRows
                filteredRows
                (categorize (getCategoryByShort (Dict.values data.categories)) (applyAllCategorizationRules data))

        newCategories =
            annotatedRows
                |> List.Extra.uniqueBy (.parsedRow >> .category)
                |> List.map
                    (.category
                        >> Maybe.andThen
                            (\c ->
                                case c of
                                    Unknown cat ->
                                        Just cat

                                    _ ->
                                        Nothing
                            )
                    )
                |> Maybe.Extra.values
                |> List.sort
    in
    AnnotatedRows csvSize nExcluded newCategories annotatedRows


annotateRows : List ParsedRow -> (ParsedRow -> Maybe Categorization) -> List AnnotatedRow
annotateRows parsedRows categorizeRow =
    parsedRows |> List.map (\row -> AnnotatedRow row (categorizeRow row))


categorize : (String -> Maybe Category) -> (String -> Maybe Category) -> ParsedRow -> Maybe Categorization
categorize categoryLookup categorizationByRules row =
    case row.category of
        Just cat_str ->
            Just (parseCategory categoryLookup cat_str)

        Nothing ->
            categorizationByRules row.description |> matchedCategorization


type Categorization
    = None
    | Known Category
    | RuleMatch Category
    | Unknown String
    | ParsingError String


matchedCategorization : Maybe Category -> Maybe Categorization
matchedCategorization cat =
    Maybe.map (\c -> RuleMatch c) cat


parseCategory : (String -> Maybe Category) -> String -> Categorization
parseCategory categoryLookup categoryName =
    case
        Parser.run CategoryParser.categoryShortNameOrEmpty categoryName
    of
        Ok Nothing ->
            None

        Ok (Just cat) ->
            case categoryLookup cat of
                Just c ->
                    Known c

                Nothing ->
                    Unknown categoryName

        Err _ ->
            ParsingError categoryName


categoryCell : AnnotatedRow -> Element msg
categoryCell annotatedRow =
    case annotatedRow.category of
        Nothing ->
            Element.none

        Just None ->
            Element.none

        Just (Known c) ->
            spread (text c.name) (iconTooltip checkMark color.darkAccent "Known Category")

        Just (RuleMatch c) ->
            spread (text c.name) (iconTooltip infoMark color.darkAccent "Matched Categorization Rule")

        Just (Unknown name) ->
            spread (text name) (iconTooltip folderPlus color.darkAccent "New category, will be created upon import.")

        Just (ParsingError invalidShortName) ->
            spread (text invalidShortName) (iconTooltip warnTriangle color.red "Cannot create category, invalid category shortname. Row will be uncategorized.")


spread : Element msg -> Element msg -> Element msg
spread a b =
    row [ width fill ] [ a, el [ width fill ] Element.none, b ]


iconTooltip : (List (Attribute msg) -> Int -> Element msg) -> Color -> String -> Element msg
iconTooltip icon color hint =
    icon [ Font.color color, tooltip onRight hint ] size.m
