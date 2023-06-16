module Components.Filter exposing (..)

import Components.Layout exposing (size)
import Element exposing (Element, el, paddingEach, paddingXY, text)
import Element.Input as Input exposing (labelLeft, labelRight, placeholder)
import Maybe.Extra
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Processing.BookEntry exposing (Categorization(..))
import Processing.Filter as Processing exposing (any, filterAccount, filterCategory, filterDescription, filterMonth, filterYear)
import Processing.Model exposing (getCategoryByShort)


type alias Model =
    { year : String
    , month : String
    , descr : String
    , category : String
    , accounts : List Account
    , onlyUncategorized : Bool
    }


init accounts =
    { year = ""
    , month = ""
    , descr = ""
    , category = ""
    , accounts = accounts
    , onlyUncategorized = False
    }


type Msg
    = Year String
    | Month String
    | Descr String
    | Category String
    | OnlyUncategorized Bool
    | AddAccount Account
    | RemoveAccount Account
    | SetAccounts (List Account)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Year year ->
            { model | year = year }

        Month month ->
            { model | month = month }

        Descr descr ->
            { model | descr = descr }

        Category cat ->
            { model | category = cat }

        AddAccount acc ->
            { model | accounts = acc :: model.accounts }

        RemoveAccount acc ->
            { model | accounts = List.filter (\a -> not <| a.id == acc.id) model.accounts }

        SetAccounts list ->
            { model | accounts = list }

        OnlyUncategorized b ->
            { model | onlyUncategorized = b }


toFilter : List Category -> Model -> List Processing.Filter
toFilter categories model =
    []
        ++ (model.year |> String.toInt |> Maybe.map filterYear |> Maybe.Extra.toList)
        ++ (model.month |> String.toInt |> Maybe.map filterMonth |> Maybe.Extra.toList)
        ++ [ model.descr |> String.trim |> filterDescription ]
        ++ [ model.accounts |> List.map filterAccount |> any ]
        ++ (getCategoryByShort categories model.category |> Maybe.map (\c -> [ filterCategory c ]) |> Maybe.withDefault [])
        ++ [ \bookEntry -> not model.onlyUncategorized || bookEntry.categorization == None ]


yearFilter : Model -> (Msg -> msg) -> Element msg
yearFilter model msg =
    Input.text []
        { onChange = msg << Year
        , text = model.year
        , placeholder = Just <| placeholder [] <| text "Year"
        , label = labelLeft [ paddingXY size.m 0 ] <| text "Year"
        }


monthFilter : Model -> (Msg -> msg) -> Element msg
monthFilter model msg =
    Input.text []
        { onChange = msg << Month
        , text = model.month
        , placeholder = Just <| placeholder [] <| text "Month"
        , label = labelLeft [ paddingXY size.m 0 ] <| text "Month"
        }


descriptionFilter : Model -> (Msg -> msg) -> Element msg
descriptionFilter model msg =
    Input.text []
        { onChange = msg << Descr
        , text = model.descr
        , placeholder = Just <| placeholder [] <| text "Description"
        , label = labelLeft [ paddingXY size.m 0 ] <| text "Description"
        }


categoryFilter : Model -> (Msg -> msg) -> Element msg
categoryFilter model msg =
    Input.text []
        { onChange = msg << Category
        , text = model.category
        , placeholder = Just <| placeholder [] <| text "Category"
        , label = labelLeft [ paddingXY size.m 0 ] <| text "Category"
        }


accountFilter : List Account -> Model -> (Msg -> msg) -> Element msg
accountFilter accounts model msg =
    Element.row []
        ([ el [ paddingXY size.m 0 ] <| text "Accounts "
         , Input.checkbox []
            { onChange =
                \on ->
                    if on then
                        msg (SetAccounts accounts)

                    else
                        msg (SetAccounts [])
            , icon = Input.defaultCheckbox
            , checked = List.length model.accounts == List.length accounts
            , label = labelRight [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text <| "All"
            }
         ]
            ++ List.map
                (\acc ->
                    accountCheckbox model acc msg
                )
                accounts
        )


accountCheckbox : Model -> Account -> (Msg -> msg) -> Element msg
accountCheckbox model acc msg =
    Input.checkbox []
        { onChange =
            \add ->
                if add then
                    msg <| AddAccount acc

                else
                    msg <| RemoveAccount acc
        , icon = Input.defaultCheckbox
        , checked = List.member acc model.accounts
        , label = labelRight [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text <| acc.name
        }


uncategorizedFilter : Model -> (Msg -> msg) -> Element msg
uncategorizedFilter model msg =
    Input.checkbox []
        { onChange = msg << OnlyUncategorized
        , icon = Input.defaultCheckbox
        , checked = model.onlyUncategorized
        , label = labelLeft [ paddingXY size.m size.xs ] <| text "Show uncategorized only"
        }
