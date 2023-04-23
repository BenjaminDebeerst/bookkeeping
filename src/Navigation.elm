module Navigation exposing (view)

import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (href, id)


view : Html msg
view =
    ul [ id "navigation" ]
        [ li [] [ a [ href "/" ] [ text "Home" ] ]
        , li [] [ a [ href "/csv-import" ] [ text "Import" ] ]
        , li [] [ a [ href "/bookings" ] [ text "Book" ] ]
        ]
