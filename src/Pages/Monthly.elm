module Pages.Monthly exposing (Model, Msg, page)

import Components.Layout as Layout exposing (formatEuro, size, style)
import Dict exposing (Dict)
import Element exposing (Element, IndexedColumn, column, el, indexedTable, paddingEach, paddingXY, shrink, spacing, text)
import Element.Input as Input exposing (labelRight)
import Gen.Params.Monthly exposing (Params)
import Page
import Persistence.Data exposing (Account, Category, Data)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)
import Processing.Filter as Filter exposing (any)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    { accountFilter : List Account
    }


init : Data -> ( Model, Cmd Msg )
init data =
    ( { accountFilter = Dict.values data.accounts }, Cmd.none )



-- UPDATE


type Msg
    = FilterAddAccount Account
    | FilterRemoveAccount Account
    | FilterAllAccounts Bool


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        FilterAddAccount acc ->
            ( { model | accountFilter = acc :: model.accountFilter }, Cmd.none )

        FilterRemoveAccount acc ->
            ( { model | accountFilter = List.filter (\a -> not <| a.id == acc.id) model.accountFilter }, Cmd.none )

        FilterAllAccounts on ->
            if on then
                ( { model | accountFilter = Dict.values data.accounts }, Cmd.none )

            else
                ( { model | accountFilter = [] }, Cmd.none )



-- VIEW


view : Data -> Model -> View Msg
view data model =
    let
        filter =
            [ model.accountFilter |> List.map Filter.filterAccount |> any ]

        aggregatedData =
            aggregate <| getEntries data filter dateAsc
    in
    Layout.page "Monthly" <|
        [ showFilters model data.accounts
        , showData data aggregatedData
        ]


showFilters : Model -> Dict Int Account -> Element Msg
showFilters model accounts =
    column [ spacing size.s ]
        [ el style.h2 <| text "Filters"
        , Element.row []
            ([ el [ paddingXY size.m 0 ] <| text "Accounts "
             , Input.checkbox []
                { onChange = FilterAllAccounts
                , icon = Input.defaultCheckbox
                , checked = List.length model.accountFilter == Dict.size accounts
                , label = labelRight [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text <| "All"
                }
             ]
                ++ List.map
                    (\acc ->
                        Input.checkbox []
                            { onChange = filterAccount acc
                            , icon = Input.defaultCheckbox
                            , checked = List.member acc model.accountFilter
                            , label = labelRight [ paddingEach { top = 0, right = size.l, bottom = 0, left = 0 } ] <| text <| acc.name
                            }
                    )
                    (Dict.values accounts)
            )
        ]


filterAccount : Account -> Bool -> Msg
filterAccount acc add =
    if add then
        FilterAddAccount acc

    else
        FilterRemoveAccount acc


showData : Data -> Aggregate -> Element msg
showData data aggregate =
    indexedTable [ spacing size.tiny ]
        { data = aggregate.rows
        , columns =
            [ { header = el style.header <| text "Month"
              , width = shrink
              , view = \i e -> el (style.row i) <| text e.month
              }
            , { header = el style.header <| text "Balance"
              , width = shrink
              , view = \i e -> el (style.row i) <| formatEuro [] e.balance
              }
            ]
                ++ categoryColumns data.categories
        }


categoryColumns : Dict Int Category -> List (IndexedColumn MonthAggregate msg)
categoryColumns categories =
    Dict.values categories
        |> List.map
            (\cat ->
                { header = el style.header <| text cat.name
                , width = shrink
                , view = \i aggregate -> el (style.row i) <| (Dict.get cat.id aggregate.entries |> Maybe.withDefault 0 |> formatEuro [])
                }
            )
