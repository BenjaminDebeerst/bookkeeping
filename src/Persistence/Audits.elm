module Persistence.Audits exposing (Audit, Audits, codec, empty, get, set, update)

import Dict exposing (Dict)
import List.Extra
import Persistence.YearMonth as YMC
import Serialize as S
import Util.YearMonth exposing (YearMonth)


type alias Audits =
    AuditsV0


get : YearMonth -> Audits -> Audit
get ym (AV0 audits) =
    audits
        |> List.Extra.find (\a -> a.yearMonth == ym)
        |> Maybe.withDefault (newAudit ym)


update : YearMonth -> (Audit -> Audit) -> Audits -> Audits
update ym fn audits =
    audits
        |> get ym
        |> fn
        |> (\updated -> set updated audits)


set : Audit -> Audits -> Audits
set audit (AV0 audits) =
    case List.Extra.findIndex (\a -> a.yearMonth == audit.yearMonth) audits of
        Just index ->
            AV0 (List.Extra.setAt index audit audits)

        Nothing ->
            AV0 (audit :: audits)


type AuditsV0
    = AV0 (List Audit)


type alias Audit =
    { yearMonth : YearMonth
    , comment : String
    , balance : Maybe Int
    , accountBalance : Dict Int Int
    }


empty : Audits
empty =
    AV0 []


newAudit : YearMonth -> Audit
newAudit ym =
    Audit ym "" Nothing Dict.empty


codec : S.Codec e Audits
codec =
    S.customType
        (\v1Encoder value ->
            case value of
                V0 record ->
                    v1Encoder record
        )
        |> S.variant1 V0 auditsV0Codec
        |> S.finishCustomType
        |> S.map
            (\value ->
                case value of
                    V0 storage ->
                        storage
            )
            V0


type AuditsVersions
    = V0 AuditsV0


auditsV0Codec : S.Codec e AuditsV0
auditsV0Codec =
    S.map (\d -> AV0 d) (\(AV0 d) -> d) (S.list auditCodec)


auditCodec : S.Codec e Audit
auditCodec =
    S.record Audit
        |> S.field .yearMonth YMC.codec
        |> S.field .comment S.string
        |> S.field .balance (S.maybe S.int)
        |> S.field .accountBalance (S.dict S.int S.int)
        |> S.finishRecord
