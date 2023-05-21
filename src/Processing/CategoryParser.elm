module Processing.CategoryParser exposing (Categorization(..), parseCategorization)

import Parser exposing (..)
import Set


type Categorization
    = Empty
    | One String
    | Multiple (List ( String, Int ))


parseCategorization : String -> Result String Categorization
parseCategorization string =
    Parser.run categorizationParser string
        |> Result.mapError (\_ -> string)


categorizationParser : Parser Categorization
categorizationParser =
    Parser.oneOf
        [ emptyParser
        , backtrackable singleParser
        , splitParser
        ]


emptyParser : Parser Categorization
emptyParser =
    Parser.succeed Empty
        |. Parser.end


singleParser : Parser Categorization
singleParser =
    Parser.succeed One
        |. Parser.spaces
        |= categoryShortName
        |. Parser.spaces
        |. Parser.end


splitParser : Parser Categorization
splitParser =
    Parser.succeed Multiple
        |= Parser.loop [] tupleParserHelper
        |. Parser.end


tupleParserHelper : List ( String, Int ) -> Parser (Step (List ( String, Int )) (List ( String, Int )))
tupleParserHelper tuples =
    Parser.oneOf
        [ Parser.succeed (\catShort amount -> Loop (tuples ++ [ ( catShort, amount ) ]))
            |. Parser.spaces
            |= categoryShortName
            |. Parser.symbol " "
            |= Parser.int
            |. Parser.spaces
        , succeed ()
            |> Parser.map (\_ -> Done tuples)
        ]


categoryShortName : Parser String
categoryShortName =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }