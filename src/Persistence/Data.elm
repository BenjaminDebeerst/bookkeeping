module Persistence.Data exposing
    ( Data
    , decode
    , empty
    , encode
    )

import Dict exposing (Dict)
import Persistence.Account as Account exposing (Account, Accounts)
import Persistence.Category as Category exposing (Categories, Category)
import Persistence.ImportProfile as ImportProfile exposing (ImportProfile, ImportProfiles)
import Persistence.RawEntry as RawEntry exposing (RawEntries, RawEntry)
import Serialize as S exposing (Error)


type alias Data =
    DataV0


type alias DataV0 =
    { rawEntries : RawEntries
    , accounts : Accounts
    , categories : Categories
    , importProfiles : ImportProfiles
    , autoIncrement : Int
    }


empty : Data
empty =
    { rawEntries = Dict.empty
    , accounts = Dict.empty
    , categories = Dict.empty
    , importProfiles = Dict.empty
    , autoIncrement = 0
    }


encode : Data -> String
encode storage =
    S.encodeToString dataCodec storage


decode : String -> Result (Error String) Data
decode value =
    case value of
        "" ->
            Ok empty

        s ->
            S.decodeFromString dataCodec (String.trim s)



-- versioning-aware encoding


type StorageVersions
    = V0 DataV0


dataCodec : S.Codec String Data
dataCodec =
    S.customType
        (\v0Encoder value ->
            case value of
                V0 record ->
                    v0Encoder record
        )
        |> S.variant1 V0 v0Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        storage
            )
            V0


v0Codec : S.Codec String Data
v0Codec =
    S.record DataV0
        |> S.field .rawEntries RawEntry.codec
        |> S.field .accounts Account.codec
        |> S.field .categories Category.codec
        |> S.field .importProfiles ImportProfile.codec
        |> S.field .autoIncrement S.int
        |> S.finishRecord
