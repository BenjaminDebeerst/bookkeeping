module Pages.Book exposing (Model, Msg, page)

import Components.Filter as Filter
import Components.Icons exposing (checkMark, triangleDown, triangleUp, warnTriangle)
import Components.Layout as Layout exposing (color, formatDate, formatEuro, formatEuroStr, size, style, tooltip, updateOrRedirectOnError, viewDataOnly)
import Components.Table as T
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Column, Element, alignLeft, alignRight, below, centerX, column, el, fill, height, indexedTable, padding, shrink, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import Page
import Parser
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data)
import Persistence.RawEntry exposing (RawEntry)
import Persistence.Storage as Storage exposing (addEntries, removeEntries)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)
import Processing.CategoryParser as Parser exposing (categorizationParser)
import Processing.Model exposing (getCategoryByShort, getEntriesAndErrors)
import Processing.Ordering exposing (Ordering, asc, dateAsc, dateDesc, desc)
import Request exposing (Request)
import Result.Extra
import Set
import Shared exposing (Model(..))
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init =
            init
                (case shared of
                    Loaded data ->
                        Dict.values data.accounts

                    Problem _ ->
                        []
                )
        , update = updateOrRedirectOnError shared req update
        , view = viewDataOnly shared view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { ordering : Ordering BookEntry
    , editCategories : Bool
    , categoryEdits : Dict String CatAttempt
    , filters : Filter.Model
    , toBeDeleted : List String
    }


type CatAttempt
    = Unknown String String
    | Known String Categorization


init : List Account -> ( Model, Cmd Msg )
init accounts =
    ( { ordering = dateAsc
      , editCategories = False
      , categoryEdits = Dict.empty
      , filters = Filter.init accounts
      , toBeDeleted = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Filter Filter.Msg
    | OrderBy (Ordering BookEntry)
    | Categorize
    | EditCategory String Int String
    | SaveCategories
    | AbortCategorize
    | Delete (List String)
    | DeleteAbort
    | DeleteConfirm (List String)


update : Data -> Msg -> Model -> ( Model, Cmd Msg )
update data msg model =
    case msg of
        Filter filterMsg ->
            ( { model | filters = Filter.update filterMsg model.filters }, Cmd.none )

        OrderBy ordering ->
            ( { model | ordering = ordering }, Cmd.none )

        Categorize ->
            ( { model | editCategories = True }, Cmd.none )

        AbortCategorize ->
            ( { model | editCategories = False }, Cmd.none )

        EditCategory id amount cat ->
            ( { model | categoryEdits = Dict.insert id (parseCategorization data amount (String.toUpper cat)) model.categoryEdits }, Cmd.none )

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
            , addEntries False editedEntries data |> Storage.store
            )

        Delete entryIds ->
            ( { model | toBeDeleted = entryIds }, Cmd.none )

        DeleteAbort ->
            ( { model | toBeDeleted = [] }, Cmd.none )

        DeleteConfirm entryIds ->
            ( { model | toBeDeleted = [] }, removeEntries entryIds data |> Storage.store )


view : Data -> Model -> View Msg
view data model =
    let
        filters =
            Filter.toFilter (Dict.values data.categories) model.filters

        ( entries, errors ) =
            getEntriesAndErrors data filters model.ordering
    in
    Layout.page "Book" <|
        [ showFilters model.filters <| Dict.values data.accounts
        , showActions model (entries |> List.map .id)
        , showData model entries
        , showErrors errors
        ]


showFilters : Filter.Model -> List Account -> Element Msg
showFilters model accounts =
    column [ spacing size.s ]
        [ el style.h2 <| text "Filters"
        , Filter.yearFilter model Filter
        , Filter.monthFilter model Filter
        , Filter.descriptionFilter model Filter
        , Filter.categoryFilter model Filter
        , Filter.accountFilter accounts model Filter
        , Filter.uncategorizedFilter model Filter
        ]


showActions : Model -> List String -> Element Msg
showActions model entryIds =
    column [ width shrink, spacing size.m ]
        [ Element.row [ spacing size.s ]
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
        , Element.row [ spacing size.s ]
            (if List.isEmpty model.toBeDeleted then
                [ Input.button style.button { onPress = Just (Delete entryIds), label = text "Delete Entries Shown" } ]

             else
                [ text <| "You're about to delete " ++ (String.fromInt <| List.length model.toBeDeleted) ++ " book entries. Sure?"
                , Input.button style.button { onPress = Just DeleteAbort, label = text "No! Get me out of here." }
                , Input.button style.button { onPress = Just (DeleteConfirm entryIds), label = text "Really delete" }
                ]
            )
        ]


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


bookDisplayCap =
    250


summary entries =
    let
        n =
            List.length entries

        sum =
            List.foldl (\e s -> s + e.amount) 0 entries

        avg =
            sum // n

        capNotice =
            if n > bookDisplayCap then
                [ "Only showing the first", String.fromInt bookDisplayCap, "entries." ]

            else
                []
    in
    String.join " "
        ([ "Found", n |> String.fromInt, "entries. Sum:", formatEuroStr sum, ". Average amount:", formatEuroStr avg, "." ]
            ++ capNotice
        )
        |> text


dataTable model entries =
    indexedTable
        [ spacing size.tiny ]
        { data = List.take bookDisplayCap entries
        , columns =
            [ T.fullStyledColumn
                (header (OrderBy dateAsc) (OrderBy dateDesc) "Date")
                (.date >> formatDate >> text)
            , T.fullStyledColumn
                (header (OrderBy (asc .amount)) (OrderBy (desc .amount)) "Amount")
                (.amount >> formatEuro)
            , T.fullStyledColumn
                (header (OrderBy (asc <| .categorization >> categorizationString Full)) (OrderBy (desc <| .categorization >> categorizationString Full)) "Category")
                (categoryCell model)
            , T.fullStyledColumn
                (header (OrderBy (asc .description)) (OrderBy (desc .description)) "Description")
                (.description >> text)
            , T.fullStyledColumn
                (header (OrderBy (asc accountName)) (OrderBy (desc accountName)) "Account")
                (.account >> .name >> text)
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


parseCategorization : Data -> Int -> String -> CatAttempt
parseCategorization data targetAmount string =
    case Parser.run categorizationParser string of
        Ok Parser.Empty ->
            Known string None

        Ok (Parser.One shortName) ->
            getCategoryByShort (Dict.values data.categories) shortName
                |> Maybe.map (Known string << Single)
                |> Maybe.withDefault (Unknown string ("Unknown category " ++ shortName))

        Ok (Parser.Multiple list) ->
            case
                list
                    |> List.map (categoryForTuple (Dict.values data.categories))
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


categoryForTuple : List Category -> ( String, Int ) -> Result String EntrySplit
categoryForTuple categories ( string, int ) =
    getCategoryByShort categories string
        |> Maybe.map (\c -> Ok <| EntrySplit c int)
        |> Maybe.withDefault (Err string)
