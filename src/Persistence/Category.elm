module Persistence.Category exposing (..)

import Dict exposing (Dict)
import Serialize as S


type alias Categories =
    Dict Int Category


type alias Category =
    { id : Int
    , name : String
    , short : String
    }



-- Codecs


codec : S.Codec String Categories
codec =
    S.dict S.int categoryCodec


categoryCodec : S.Codec String Category
categoryCodec =
    S.record Category
        |> S.field .id S.int
        |> S.field .name S.string
        |> S.field .short S.string
        |> S.finishRecord
