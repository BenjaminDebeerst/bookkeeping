module Processing.CategorizationRules exposing (applyAllCategorizationRules)

import Dict
import Maybe.Extra exposing (orElseLazy)
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data)
import Regex


applyCategorizationRules : List (String -> Maybe Category) -> String -> Maybe Category
applyCategorizationRules matchers desc =
    List.foldl (\matcher found -> found |> orElseLazy (\() -> matcher desc))
        Nothing
        matchers


patternToMatcher : Category -> String -> (String -> Maybe Category)
patternToMatcher category pattern =
    case Regex.fromString pattern of
        Just regex ->
            \desc ->
                if Regex.contains regex desc then
                    Just category

                else
                    Nothing

        Nothing ->
            \_ -> Nothing


toCategorizationRules : Data -> List (String -> Maybe Category)
toCategorizationRules data =
    List.concatMap (\category -> List.map (\pattern -> patternToMatcher category pattern) category.rules) (Dict.values data.categories)


applyAllCategorizationRules : Data -> (String -> Maybe Category)
applyAllCategorizationRules data =
    \desc -> applyCategorizationRules (toCategorizationRules data) desc
