module CsvParser exposing (..)

import Expect
import Processing.Csv as Csv
import Test exposing (Test, describe, test)
import Time.Date as Date


csvText =
    "This;is the first line that shall be skipped\u{000D}\n"
        ++ oneCsvLine
        ++ "\u{000D}\n"
        ++ oneCsvLine
        ++ "\u{000D}\n"


oneCsvLine =
    "foo;bar;baz;boz;1.1.1970;bez;this;not;that;is the;description;1234,56"


expectedRow =
    { date = Date.date 1970 1 1
    , description = "this\nis the\ndescription"
    , amount = 123456
    , raw = oneCsvLine ++ "\u{000D}"
    }


parse_csv : Test
parse_csv =
    describe "Parse CSV text"
        [ test "naive 'parser'" <|
            \_ -> Csv.parseCsvLine oneCsvLine |> Expect.equal (Ok { date = Date.date 1970 1 1, description = "this\nis the\ndescription", amount = 123456, raw = oneCsvLine })
        , test "parsing the entire thing" <|
            \_ -> Csv.parseEntries (String.split "\n" csvText) |> Expect.equal [ expectedRow, expectedRow ]
        ]
