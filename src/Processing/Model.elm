module Processing.Model exposing (getEntries)

import Dict
import Persistence.Data exposing (Account, Data, RawEntry)
import Processing.BookEntry exposing (BookEntry)
import Processing.Csv exposing (Row, parseCsvLine)
import Processing.Filter exposing (Filter, all)
import Processing.Ordering exposing (Ordering)


getEntries : Data -> List Filter -> Ordering BookEntry -> List BookEntry
getEntries data filters order =
    Dict.values data.rawEntries
        |> List.map parseToEntry
        |> List.filterMap (enrichRow data)
        |> List.filter (all filters)
        |> List.sortWith order


type alias Entry =
    { id : String
    , row : Maybe Row
    , accountId : Int
    }


parseToEntry : RawEntry -> Entry
parseToEntry raw =
    parseCsvLine raw.line
        |> Result.toMaybe
        |> (\row -> Entry raw.id row raw.accountId)


enrichRow : Data -> Entry -> Maybe BookEntry
enrichRow data entry =
    Maybe.map5 BookEntry
        (Just entry.id)
        (Maybe.map .date entry.row)
        (Maybe.map .description entry.row)
        (Maybe.map .amount entry.row)
        (Dict.get entry.accountId data.accounts)
