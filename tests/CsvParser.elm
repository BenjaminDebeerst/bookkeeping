module CsvParser exposing (..)

import Expect
import Persistence.Data exposing (DateFormat(..))
import Processing.CsvParser as Csv
import Test exposing (Test, describe, test)
import Time.Date as Date


profile =
    { id = 0
    , name = "Test"
    , splitAt = ';'
    , dateField = 4
    , descrFields = [ 6, 9, 10 ]
    , amountField = 11
    , dateFormat = DDMMYYYY '.'
    , categoryField = Nothing
    }


{-| intentionally inconsistent line breaks
-}
csvText =
    "This;is the first line that shall be skipped, and the number of columns doesn't matter\n"
        ++ oneCsvLine
        ++ "\u{000D}\n"
        ++ oneCsvLine


oneCsvLine =
    "foo;bar;baz;boz;31.7.1970;bez;this;not;that;\"is the;\";description;1234,56"


expectedRow =
    { date = Date.date 1970 7 31
    , description = "this\nis the;\ndescription"
    , amount = 123456
    , category = Nothing
    , rawLine = oneCsvLine
    }


parse_csv : Test
parse_csv =
    describe "CSV Parser"
        [ test "parses CSV" <|
            \_ -> Csv.parse profile csvText |> Expect.equal (Ok [ expectedRow, expectedRow ])
        , test "ISO date format" <|
            \_ ->
                let
                    mod =
                        String.replace "31.7.1970" "1970-07-31"
                in
                Csv.parse { profile | dateFormat = YYYYMMDD '-' } (String.replace "31.7.1970" "1970-07-31" csvText)
                    |> Expect.equal
                        (Ok
                            [ { expectedRow | rawLine = mod expectedRow.rawLine }
                            , { expectedRow | rawLine = mod expectedRow.rawLine }
                            ]
                        )
        , test "slash date format" <|
            \_ ->
                let
                    mod =
                        String.replace "31.7.1970" "31/07/1970"
                in
                Csv.parse { profile | dateFormat = DDMMYYYY '/' } (mod csvText)
                    |> Expect.equal
                        (Ok
                            [ { expectedRow | rawLine = mod expectedRow.rawLine }
                            , { expectedRow | rawLine = mod expectedRow.rawLine }
                            ]
                        )
        , test "funky date format" <|
            \_ ->
                let
                    mod =
                        String.replace "31.7.1970" "31|07|1970"
                in
                Csv.parse { profile | dateFormat = DDMMYYYY '|' } (mod csvText)
                    |> Expect.equal
                        (Ok
                            [ { expectedRow | rawLine = mod expectedRow.rawLine }
                            , { expectedRow | rawLine = mod expectedRow.rawLine }
                            ]
                        )
        ]
