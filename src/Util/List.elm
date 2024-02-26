module Util.List exposing (..)


partitionWith : (a -> Order) -> List a -> ( List a, List a, List a )
partitionWith order list =
    let
        step x ( lts, eqs, gts ) =
            case order x of
                LT ->
                    ( x :: lts, eqs, gts )

                EQ ->
                    ( lts, x :: eqs, gts )

                GT ->
                    ( lts, eqs, x :: gts )
    in
    List.foldr step ( [], [], [] ) list
