module Processing.CategorizationRules exposing (applyCategorizationRules, applyCategorizationRule, applyAllCategorizationRules)

import Dict
import Maybe.Extra exposing (orElseLazy)
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data)
import Regex


applyCategorizationRule : Category -> String -> String -> Maybe Category
applyCategorizationRule category desc pattern =
    case (Maybe.map (\r -> Regex.contains r desc) (Regex.fromString pattern)) of
        Just result ->
            if result then
                Just category
            else
                Nothing
        _ -> Nothing

applyCategorizationRules : Category -> String -> Maybe Category
applyCategorizationRules category desc =
    List.foldl (\pattern found -> found |> orElseLazy (\() -> (applyCategorizationRule category desc pattern)))
                    Nothing
                    category.rules

applyAllCategorizationRules : Data -> String -> Maybe Category
applyAllCategorizationRules data desc =
    List.foldl (\category found -> found |> orElseLazy (\() -> (applyCategorizationRules category desc)))
                    Nothing
                    (Dict.values data.categories)
