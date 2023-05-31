module CsvParser exposing (..)

import Expect
import Processing.CsvParser as Csv
import Test exposing (Test, describe, test)
import Time.Date as Date


csvText =
    "This;is the first line that shall be skipped, and the number of columns doesn't matter\u{000D}\n"
        ++ oneCsvLine
        ++ "\u{000D}\n"
        ++ oneCsvLine


oneCsvLine =
    "foo;bar;baz;boz;1.1.1970;bez;this;not;that;\"is the;\";description;1234,56"


expectedRow =
    { date = Date.date 1970 1 1
    , description = "this\nis the;\ndescription"
    , amount = 123456
    }


parse_csv : Test
parse_csv =
    describe "CSV Parser"
        [ test "parses CSV" <|
            \_ -> Csv.parse csvText |> Expect.equal (Ok [ expectedRow, expectedRow ])
        ]
