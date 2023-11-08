module Persistence.RawEntry exposing (Categorization(..), RawEntries, RawEntry, SplitCatEntry, codec, rawEntry, sha1)

import Dict exposing (Dict)
import Persistence.Category exposing (Category)
import SHA1
import Serialize as S
import Time.Date as Date exposing (Date)


type alias RawEntries =
    Dict String RawEntry


type alias RawEntry =
    { id : String
    , line : String
    , date : Date
    , amount : Int
    , description : String
    , accountId : Int
    , importProfile : Int
    , categorization : Maybe Categorization
    }


type Categorization
    = Single Int
    | Split (List SplitCatEntry)


type alias SplitCatEntry =
    { id : Int, amount : Int }


rawEntry : Int -> Int -> String -> Date -> Int -> String -> Maybe Category -> RawEntry
rawEntry accountId profileId line date amount description category =
    { id = sha1 line
    , line = line
    , date = date
    , amount = amount
    , description = description
    , accountId = accountId
    , importProfile = profileId
    , categorization = Maybe.map (.id >> Single) category
    }


sha1 : String -> String
sha1 s =
    SHA1.fromString s |> SHA1.toHex



-- Codecs


codec : S.Codec String RawEntries
codec =
    S.dict S.string rawEntryCodec


rawEntryCodec =
    S.record RawEntry
        |> S.field .id S.string
        |> S.field .line S.string
        |> S.field .date dateCodec
        |> S.field .amount S.int
        |> S.field .description S.string
        |> S.field .accountId S.int
        |> S.field .importProfile S.int
        |> S.field .categorization (S.maybe categorizationCodec)
        |> S.finishRecord


dateCodec : S.Codec String Date
dateCodec =
    S.triple S.int S.int S.int
        |> S.map Date.fromTuple Date.toTuple


categorizationCodec : S.Codec String Categorization
categorizationCodec =
    S.customType
        (\singleEncoder splitEncoder value ->
            case value of
                Single id ->
                    singleEncoder id

                Split lst ->
                    splitEncoder lst
        )
        |> S.variant1 Single S.int
        |> S.variant1 Split splitCategorizationCodec
        |> S.finishCustomType


splitCategorizationCodec : S.Codec String (List SplitCatEntry)
splitCategorizationCodec =
    S.list
        (S.record SplitCatEntry
            |> S.field .id S.int
            |> S.field .amount S.int
            |> S.finishRecord
        )
