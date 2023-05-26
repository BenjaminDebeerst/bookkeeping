module Pages.Monthly exposing (Model, Msg, page)

import Dict exposing (Dict)
import Element exposing (IndexedColumn, el, indexedTable, shrink, spacing, text)
import Gen.Params.Monthly exposing (Params)
import Layout exposing (formatEuro, size, style)
import Page
import Persistence.Data exposing (Category, Data)
import Processing.Aggregation exposing (Aggregate, MonthAggregate, aggregate)
import Processing.Model exposing (getEntries)
import Processing.Ordering exposing (dateAsc)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Data -> Model -> View Msg
view data _ =
    let
        entries =
            getEntries data [] dateAsc

        accounts =
            Dict.values data.accounts

        aggregatedData : List Aggregate
        aggregatedData =
            accounts
                |> List.map (aggregate entries)
    in
    Layout.page "Monthly" <|
        (aggregatedData
            |> List.map
                (\aggregate ->
                    [ el style.h2 <| text <| aggregate.account.name
                    , indexedTable [ spacing size.tiny ]
                        { data = aggregate.rows
                        , columns =
                            [ { header = el style.header <| text "Month"
                              , width = shrink
                              , view = \i e -> el (style.row i) <| text e.month
                              }
                            ]
                                ++ categoryColumns data.categories
                        }
                    ]
                )
            |> List.concat
        )


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
