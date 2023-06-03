module Pages.Book exposing (Model, Msg, page)

import Components.Icons exposing (checkMark, triangleDown, triangleUp, warnTriangle)
import Components.Layout as Layout exposing (color, formatDate, formatEuro, formatEuroStr, size, style, tooltip)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Column, Element, alignLeft, alignRight, below, centerX, column, el, fill, height, indexedTable, padding, paddingEach, paddingXY, shrink, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelLeft, labelRight, placeholder)
import Maybe.Extra
import Page
import Parser
import Persistence.Data exposing (Account, Category, Data, RawEntry)
import Persistence.Storage as Storage exposing (addEntries)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)
import Processing.CategoryParser as Parser exposing (categorizationParser)
import Processing.Filter as Filter exposing (Filter, any, filterAccount, filterCategory, filterDescription, filterMonth, filterYear)
import Processing.Model exposing (getCategoryByShort, getEntriesAndErrors)
import Processing.Ordering exposing (Ordering, asc, dateAsc, dateDesc, desc)
import Request exposing (Request)
import Result.Extra
import Set
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { year : String
    , month : String
    , descr : String
    , categoryFilter : String
    , accountFilter : List Account
    , ordering : Ordering BookEntry
    , editCategories : Bool
    , categoryEdits : Dict String CatAttempt
    , onlyUncategorized : Bool
    }


type CatAttempt
    = Unknown String String
    | Known String Categorization


init : Data -> ( Model, Cmd Msg )
init data =
    ( { year = ""
      , month = ""
      , descr = ""
      , categoryFilter = ""
      , accountFilter = Dict.values data.accounts
      , ordering = dateAsc
      , editCategories = False
      , categoryEdits = Dict.empty
      , onlyUncategorized = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FilterYear String
    | FilterMonth String
    | FilterDescr String
    | FilterCategory String
    | FilterAddAccount Account
    | FilterRemoveAccount Account
    | FilterAllAccounts Bool
    | OnlyUncategorized Bool
    | OrderBy (Ordering BookEntry)
    | Categorize
    | EditCategory String Int String
    | SaveCategories
    | AbortCategorize


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        FilterYear year ->
            ( { model | year = year }, Cmd.none )

        FilterMonth month ->
            ( { model | month = month }, Cmd.none )

        FilterDescr descr ->
            ( { model | descr = descr }, Cmd.none )

        FilterCategory cat ->
            ( { model | categoryFilter = cat }, Cmd.none )

        FilterAddAccount acc ->
            ( { model | accountFilter = acc :: model.accountFilter }, Cmd.none )

        FilterRemoveAccount acc ->
            ( { model | accountFilter = List.filter (\a -> not <| a.id == acc.id) model.accountFilter }, Cmd.none )

        FilterAllAccounts on ->
            if on then
                ( { model | accountFilter = Dict.values data.accounts }, Cmd.none )

            else
                ( { model | accountFilter = [] }, Cmd.none )

        OnlyUncategorized b ->
            ( { model | onlyUncategorized = b }, Cmd.none )

        OrderBy ordering ->
            ( { model | ordering = ordering }, Cmd.none )

        Categorize ->
            ( { model | editCategories = True }, Cmd.none )

        AbortCategorize ->
            ( { model | editCategories = False }, Cmd.none )

        EditCategory id amount cat ->
            ( { model | categoryEdits = Dict.insert id (parseCategorization data amount cat) model.categoryEdits }, Cmd.none )

        SaveCategories ->
            let
                entryCategorizations : Dict String Categorization
                entryCategorizations =
                    Dict.Extra.filterMap
                        (\_ ca ->
                            case ca of
                                Unknown "" _ ->
                                    Just None

                                Unknown _ _ ->
                                    Nothing

                                Known _ cat ->
                                    Just cat
                        )
                        model.categoryEdits

                alteredCategories =
                    Dict.Extra.filterMap (\k v -> Dict.get k data.rawEntries |> Maybe.map (\e -> { e | categorization = toPersistence v })) entryCategorizations

                editedEntries : List RawEntry
                editedEntries =
                    alteredCategories
                        |> Dict.values
            in
            ( { model | editCategories = False, categoryEdits = Dict.empty }
            , addEntries editedEntries data |> Storage.store
            )


view : Data -> Model -> View Msg
view data model =
    let
        filters =
            []
                ++ (model.year |> String.toInt |> Maybe.map filterYear |> Maybe.Extra.toList)
                ++ (model.month |> String.toInt |> Maybe.map filterMonth |> Maybe.Extra.toList)
                ++ [ model.descr |> String.trim |> filterDescription ]
                ++ (getCategoryByShort data model.categoryFilter |> Maybe.map (\c -> [ filterCategory c ]) |> Maybe.withDefault [])
                ++ [ model.accountFilter |> List.map Filter.filterAccount |> any ]
                ++ [ \bookEntry -> not model.onlyUncategorized || bookEntry.categorization == None ]

        ( entries, errors ) =
            getEntriesAndErrors data filters model.ordering
    in
    Layout.page "Book" <|
        [ showFilters model data.accounts
        , showActions model
        , showData model entries
        , showErrors errors
        ]


showFilters : Model -> Dict Int Account -> Element Msg
showFilters model accounts =
    column [ spacing size.s ]
        [ el style.h2 <| text "Filters"
        , Input.text []
            { onChange = FilterYear
            , text = model.year
            , placeholder = Just <| placeholder [] <| text "Year"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Year"
            }
        , Input.text []
            { onChange = FilterMonth
            , text = model.month
            , placeholder = Just <| placeholder [] <| text "Month"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Month"
            }
        , Input.text []
            { onChange = FilterDescr
            , text = model.descr
            , placeholder = Just <| placeholder [] <| text "Description"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Description"
            }
        , Input.text []
            { onChange = FilterCategory
            , text = model.categoryFilter
            , placeholder = Just <| placeholder [] <| text "Category"
            , label = labelLeft [ paddingXY size.m 0 ] <| text "Category"
            }
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
        , Input.checkbox []
            { onChange = OnlyUncategorized
            , icon = Input.defaultCheckbox
            , checked = model.onlyUncategorized
            , label = labelLeft [ paddingXY size.m size.xs ] <| text "Show uncategorized only"
            }
        ]


filterAccount : Account -> Bool -> Msg
filterAccount acc add =
    if add then
        FilterAddAccount acc

    else
        FilterRemoveAccount acc


showActions : Model -> Element Msg
showActions model =
    Element.row [ spacing size.s ]
        ([]
            ++ [ Input.button style.button { onPress = Just Categorize, label = text "Edit Categories" } ]
            ++ (if model.editCategories then
                    [ Input.button style.button { onPress = Just SaveCategories, label = text "Save Category Edits" }
                    , Input.button style.button { onPress = Just AbortCategorize, label = text "Abort" }
                    ]

                else
                    []
               )
        )


showData : Model -> List BookEntry -> Element Msg
showData model entries =
    column [ width shrink, spacing size.m ]
        [ summary entries
        , dataTable model entries
        , maybeNoEntries <| List.length entries
        ]


showErrors : List String -> Element msg
showErrors strings =
    if List.isEmpty strings then
        Element.none

    else
        el [] (text <| "There were " ++ (List.length strings |> String.fromInt) ++ " book entries in the DB that could not be parsed. Something is wrong.")


summary entries =
    let
        n =
            List.length entries

        sum =
            List.foldl (\e s -> s + e.amount) 0 entries

        avg =
            sum // n
    in
    el [] <|
        text <|
            String.join " "
                [ "Showing"
                , n |> String.fromInt
                , "entries. Sum:"
                , sum |> formatEuroStr
                , ". Average amount:"
                , formatEuroStr avg
                , "."
                ]


dataTable model entries =
    indexedTable [ spacing size.tiny ]
        { data = entries
        , columns =
            [ { header = header (OrderBy dateAsc) (OrderBy dateDesc) "Date"
              , width = shrink
              , view = \i e -> row i <| text <| formatDate e.date
              }
            , { header = header (OrderBy (asc .amount)) (OrderBy (desc .amount)) "Amount"
              , width = shrink
              , view = \i e -> row i <| formatEuro [] e.amount
              }
            , { header = header (OrderBy (asc <| .categorization >> categorizationString Full)) (OrderBy (desc <| .categorization >> categorizationString Full)) "Category"
              , width = shrink
              , view = \i e -> row i <| categoryCell model e
              }
            , { header = header (OrderBy (asc .description)) (OrderBy (desc .description)) "Description"
              , width = shrink
              , view = \i e -> row i <| text e.description
              }
            , { header = header (OrderBy (asc accountName)) (OrderBy (desc accountName)) "Account"
              , width = shrink
              , view = \i e -> row i <| text e.account.name
              }
            ]
        }


categoryCell : Model -> BookEntry -> Element Msg
categoryCell model entry =
    if model.editCategories then
        Element.row []
            [ Input.text []
                { onChange = EditCategory entry.id entry.amount
                , text = categoryInputText model.categoryEdits entry
                , placeholder = Nothing
                , label = labelHidden "Categorization"
                }
            , categoryEditAnnotation model.categoryEdits entry
            ]

    else
        text <| categorizationString Full entry.categorization


categoryEditAnnotation : Dict String CatAttempt -> BookEntry -> Element Msg
categoryEditAnnotation categoryEdits entry =
    case Dict.get entry.id categoryEdits of
        Just (Unknown _ error) ->
            warnTriangle
                [ padding size.xs
                , Font.color color.red
                , tooltip below error
                ]
                size.l

        Just (Known _ _) ->
            checkMark [ padding size.xs, Font.color color.darkAccent ] size.l

        Nothing ->
            case entry.categorization of
                None ->
                    Element.none

                _ ->
                    checkMark [ padding size.xs, Font.color color.darkAccent ] size.l


categoryInputText : Dict String CatAttempt -> BookEntry -> String
categoryInputText categoryEdits entry =
    case Dict.get entry.id categoryEdits of
        Just (Unknown edit _) ->
            edit

        Just (Known edit _) ->
            edit

        Nothing ->
            categorizationString Short entry.categorization


type CategoryPresentation
    = Full
    | Short


categorizationString : CategoryPresentation -> Categorization -> String
categorizationString p c =
    case ( c, p ) of
        ( None, _ ) ->
            ""

        ( Single cat, Short ) ->
            cat.short

        ( Single cat, Full ) ->
            cat.name

        ( Split cats, Short ) ->
            cats
                |> List.map (\s -> s.category.short ++ " " ++ String.fromInt s.amount)
                |> List.sort
                |> String.join " "

        ( Split cats, Full ) ->
            cats
                |> List.map (\es -> es.category.name ++ " " ++ formatEuroStr es.amount)
                |> List.sort
                |> String.join "\n"


accountName : BookEntry -> String
accountName e =
    e.account.name


maybeNoEntries n =
    if n == 0 then
        el [ centerX, Font.italic, padding size.s ] <| text "No book entries found"

    else
        Element.none


header : msg -> msg -> String -> Element msg
header up down s =
    Element.row style.header
        [ el [ alignLeft ] <| text s
        , column
            [ alignRight, height fill, spacing size.xxs ]
            [ triangleUp [ onClick up ] size.s
            , triangleDown [ onClick down ] size.s
            ]
        ]


row : Int -> Element msg -> Element msg
row i e =
    el (style.row i) e


parseCategorization : Data -> Int -> String -> CatAttempt
parseCategorization data targetAmount string =
    case Parser.run categorizationParser string of
        Ok Parser.Empty ->
            Known string None

        Ok (Parser.One shortName) ->
            getCategoryByShort data shortName
                |> Maybe.map (Known string << Single)
                |> Maybe.withDefault (Unknown string ("Unknown category " ++ shortName))

        Ok (Parser.Multiple list) ->
            case
                list
                    |> List.map (categoryForTuple data)
                    |> Result.Extra.partition
            of
                ( categories, [] ) ->
                    let
                        remainder =
                            targetAmount - (categories |> List.map .amount |> List.sum)
                    in
                    if remainder == 0 then
                        Known string (Split categories)

                    else
                        Unknown string ("Remaining unallocated amount: " ++ formatEuroStr remainder)

                ( _, unknowns ) ->
                    let
                        cats =
                            Set.fromList unknowns
                                |> Set.toList
                                |> List.sort
                                |> String.join ", "
                    in
                    if List.length unknowns == 1 then
                        Unknown string ("Unknown category " ++ cats)

                    else
                        Unknown string ("Unknown categories: " ++ cats)

        Err _ ->
            Unknown string "Unrecognized format"


categoryForTuple : Data -> ( String, Int ) -> Result String EntrySplit
categoryForTuple data ( string, int ) =
    getCategoryByShort data string
        |> Maybe.map (\c -> Ok <| EntrySplit c int)
        |> Maybe.withDefault (Err string)
