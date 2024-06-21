module Persistence.YearMonth exposing (codec)

import Serialize as S
import Util.YearMonth as YearMonth exposing (YearMonth)


{-| YearMonth is a simple integer and could be encoded as such.
Encoding its components instead leads to higher human-readability
of the encoded json data. This is not strictly required, but improves
the portability of the serialization format.
-}
codec : S.Codec e YearMonth
codec =
    S.tuple S.int S.int
        |> S.map (\( y, m ) -> YearMonth.new y m) YearMonth.components
