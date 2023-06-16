module Persistence.Category exposing (..)

import Serialize as S


type alias Category =
    { id : Int
    , name : String
    , short : String
    }


categoryCodec : S.Codec String Category
categoryCodec =
    S.record Category
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.finishRecord
