module Processing.CategoryParser exposing (Categorization(..), categorizationParser, categoryShortNameOnly, categoryShortNameOrEmpty)

import Parser exposing (..)
import Set


type Categorization
    = Empty
    | One String
    | Multiple (List ( String, Int ))


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
            |. Parser.spaces
            |= oneOf
                [ succeed negate
                    |. symbol "-"
                    |= int
                , int
                ]
            |. Parser.spaces
        , succeed ()
            |> Parser.map (\_ -> Done tuples)
        ]


categoryShortName : Parser String
categoryShortName =
    Parser.variable
        { start = Char.isUpper
        , inner = \c -> Char.isUpper c || Char.isDigit c
        , reserved = Set.empty
        }


categoryShortNameOnly : Parser String
categoryShortNameOnly =
    Parser.succeed identity
        |. Parser.spaces
        |= categoryShortName
        |. Parser.spaces
        |. Parser.end


categoryShortNameOrEmpty : Parser (Maybe String)
categoryShortNameOrEmpty =
    Parser.oneOf
        [ emptyParser
        , singleParser
        ]
        |> Parser.map
            (\cat ->
                case cat of
                    Empty ->
                        Nothing

                    One c ->
                        Just c

                    -- the parsers above never return Multiple, but the match needs to be exhaustive
                    Multiple _ ->
                        Nothing
            )
