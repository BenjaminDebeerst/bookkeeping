module Pages.Book exposing (Model, Msg, page)

import Components.Filter as Filter
import Components.Icons exposing (checkMark, edit, triangleDown, triangleUp, warnTriangle)
import Components.Input exposing (button)
import Components.Notification as Notification exposing (Notification, delay)
import Components.Table as T
import Components.Tooltip exposing (tooltip)
import Config exposing (color, size, style)
import Dict exposing (Dict)
import Dict.Extra
import Effect exposing (Effect)
import Element exposing (Attribute, Column, Element, alignLeft, alignRight, below, centerX, column, el, fill, height, indexedTable, padding, paragraph, row, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import Layouts
import List.Extra
import Page exposing (Page)
import Parser
import Persistence.Account exposing (Account)
import Persistence.Category exposing (Category)
import Persistence.Data exposing (Data)
import Persistence.RawEntry exposing (RawEntry)
import Persistence.Storage exposing (editCategory, removeEntries, updateEntries)
import Processing.BookEntry exposing (BookEntry, Categorization(..), EntrySplit, toPersistence)
import Processing.CategoryParser as Parser exposing (categorizationParser)
import Processing.Model exposing (getCategoryByShort, getEntriesAndErrors)
import Processing.Ordering exposing (Ordering, asc, dateAsc, dateDesc, desc)
import Result.Extra
import Route exposing (Route)
import Set
import Shared exposing (dataSummary)
import Shared.Model
import Util.Formats exposing (formatDate, formatEuro, formatEuroStr)
import Util.Layout exposing (dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init =
            case shared of
                Shared.Model.None ->
                    init [] [] []

                Shared.Model.Problem _ ->
                    init [] [] []

                Shared.Model.Loaded data ->
                    init (Dict.values data.accounts) (Dict.values data.categories) (Dict.values data.rawEntries.entries)
        , update = dataUpdate shared update
        , view = dataView shared "Book" view
        , subscriptions = \_ -> Sub.none
        }
        |> Page.withLayout (\_ -> Layouts.Tabs { dataSummary = dataSummary shared })


type alias Model =
    { notification : Notification Msg
    , ordering : Ordering BookEntry
    , editing : Bool
    , categoryEdits : Dict Int CatAttempt
    , commentEdits : Dict Int String
    , filters : Filter.Model Msg
    , toBeDeleted : List Int
    }


type CatAttempt
    = Unknown String String
    | Known String Categorization


init : List Account -> List Category -> List RawEntry -> () -> ( Model, Effect Msg )
init accounts categories entries _ =
    ( { notification = Notification.None
      , ordering = dateAsc
      , editing = False
      , categoryEdits = Dict.empty
      , commentEdits = Dict.empty
      , filters = Filter.init accounts categories (List.map .date entries) Filter
      , toBeDeleted = []
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = Filter Filter.Msg
    | ApplyPattern Category
    | SavePattern String Category
    | OrderBy (Ordering BookEntry)
    | Edit
    | EditCategory Int Int String
    | EditComment Int String
    | SaveEdits
    | AbortEdits
    | Delete (List Int)
    | DeleteAbort
    | DeleteConfirm (List Int)
    | Restore Data Model
    | ClearNotification


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        Filter filterMsg ->
            let
                ( filters, effect ) =
                    Filter.update filterMsg model.filters
            in
            ( { model | filters = filters }, effect )

        SavePattern regex cat ->
            let
                updatedCat =
                    { cat | rules = cat.rules ++ [ regex ] }
            in
            ( { model | notification = Notification.Info [ text "Rule Saved" ] }
            , Effect.batch [ editCategory updatedCat data |> Effect.store, delay 5 ClearNotification ]
            )

        ApplyPattern cat ->
            let
                ( entries, _ ) =
                    getEntriesAndErrors data (Filter.toEntryFilter model.filters) model.ordering

                modified =
                    entries
                        |> List.filterMap (\e -> Dict.get e.id data.rawEntries.entries)
                        |> List.map (\e -> ( e.id, { e | categorization = toPersistence (Single cat) } ))
                        |> Dict.fromList

                notification =
                    if Dict.isEmpty modified then
                        Notification.None

                    else
                        undo data model (String.concat [ "Applied category '", cat.name, "' to ", String.fromInt <| Dict.size modified, " entries." ])
            in
            ( { model | notification = notification }
            , Effect.batch [ updateEntries modified data |> Effect.store, delay 5 ClearNotification ]
            )

        OrderBy ordering ->
            ( { model | ordering = ordering }, Effect.none )

        Edit ->
            ( { model | editing = True }, Effect.none )

        AbortEdits ->
            ( { model | editing = False, categoryEdits = Dict.empty, commentEdits = Dict.empty }, Effect.none )

        EditCategory id amount cat ->
            ( { model | categoryEdits = Dict.insert id (parseCategorization data amount (String.toUpper cat)) model.categoryEdits }, Effect.none )

        EditComment id comment ->
            ( { model | commentEdits = Dict.insert id comment model.commentEdits }, Effect.none )

        SaveEdits ->
            let
                entryCategorizations : Dict Int Categorization
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

                alteredEntryIds =
                    Dict.keys entryCategorizations ++ Dict.keys model.commentEdits |> Set.fromList |> Set.toList

                alteredEntries : Dict Int RawEntry
                alteredEntries =
                    alteredEntryIds
                        |> List.filterMap
                            (\id ->
                                Dict.get id data.rawEntries.entries
                                    |> Maybe.map (\e -> { e | comment = Dict.get id model.commentEdits |> Maybe.withDefault e.comment })
                                    |> Maybe.map (\e -> { e | categorization = Dict.get id entryCategorizations |> Maybe.map toPersistence |> Maybe.withDefault e.categorization })
                                    |> Maybe.map (\e -> ( id, e ))
                            )
                        |> Dict.fromList

                notification =
                    if List.length alteredEntryIds == 0 then
                        Notification.None

                    else
                        undo data model (String.concat [ "Applied edits for ", String.fromInt <| List.length alteredEntryIds, " entries." ])
            in
            ( { model
                | editing = False
                , categoryEdits = Dict.empty
                , notification = notification
              }
            , Effect.batch [ updateEntries alteredEntries data |> Effect.store, delay 5 ClearNotification ]
            )

        Delete entryIds ->
            ( { model | toBeDeleted = entryIds }, Effect.none )

        DeleteAbort ->
            ( { model | toBeDeleted = [] }, Effect.none )

        DeleteConfirm entryIds ->
            ( { model | toBeDeleted = [], notification = undo data { model | toBeDeleted = [] } (String.concat [ "Deleted ", String.fromInt (List.length entryIds), " entries." ]) }, removeEntries entryIds data |> Effect.store )

        Restore prevData prevModel ->
            ( prevModel, Effect.store prevData )

        ClearNotification ->
            ( { model | notification = Notification.None }, Effect.none )


view : Data -> Model -> Element Msg
view data model =
    let
        filters =
            Filter.toEntryFilter model.filters

        ( entries, errors ) =
            getEntriesAndErrors data filters model.ordering

        filteredCategories =
            entries |> List.concatMap entryCategories |> List.Extra.uniqueBy .id

        filterModel =
            model.filters
    in
    column [ spacing size.m, width fill ]
        [ Notification.showNotification model.notification
        , showFilters { filterModel | filterCategories = filteredCategories } (Dict.values data.accounts)
        , showActions model (entries |> List.map .id)
        , showData model entries
        , showErrors errors
        ]


entryCategories : BookEntry -> List Category
entryCategories bookEntry =
    case bookEntry.categorization of
        None ->
            []

        Single cat ->
            [ cat ]

        Split cats ->
            cats |> List.map .category


undo : Data -> Model -> String -> Notification Msg
undo data model message =
    Notification.Info
        [ text message
        , button (Restore data model) "Undo"
        ]


showFilters : Filter.Model Msg -> List Account -> Element Msg
showFilters model accounts =
    column [ spacing size.s ]
        [ Filter.accountFilter accounts model
        , Filter.dateRangeFilter model
        , Filter.descriptionFilter ApplyPattern SavePattern model
        , Filter.categoryFilter model
        ]


showActions : Model -> List Int -> Element Msg
showActions model entryIds =
    row [ spacing size.s, width fill ]
        (if model.editing then
            [ Input.button style.button { onPress = Just AbortEdits, label = text "Abort" }
            , Input.button style.button { onPress = Just SaveEdits, label = text "Save Edits" }
            ]

         else if not (List.isEmpty model.toBeDeleted) then
            [ text <| "You're about to delete " ++ (String.fromInt <| List.length model.toBeDeleted) ++ " book entries. Sure?"
            , Input.button style.button { onPress = Just DeleteAbort, label = text "No! Get me out of here." }
            , Input.button style.button { onPress = Just (DeleteConfirm entryIds), label = text "Really delete" }
            ]

         else
            [ Input.button style.button { onPress = Just (Delete entryIds), label = text "Delete Entries Shown" }, Input.button style.button { onPress = Just Edit, label = text "Edit" } ]
        )


showData : Model -> List BookEntry -> Element Msg
showData model entries =
    column [ width fill, spacing size.m ]
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
        [ spacing size.tiny, width fill ]
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
                (header (OrderBy (asc .comment)) (OrderBy (desc .comment)) "Comment")
                (commentCell model)
            , T.fullStyledColumn
                (header (OrderBy (asc .description)) (OrderBy (desc .description)) "Description")
                (.description >> text >> (\t -> paragraph [] [ t ]))
                |> T.width fill
            , T.fullStyledColumn
                (header (OrderBy (asc accountName)) (OrderBy (desc accountName)) "Account")
                (.account >> .name >> text)
            ]
        }


categoryCell : Model -> BookEntry -> Element Msg
categoryCell model entry =
    if model.editing then
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


commentCell : Model -> BookEntry -> Element Msg
commentCell model entry =
    if model.editing then
        Input.text []
            { onChange = EditComment entry.id
            , text = Dict.get entry.id model.commentEdits |> Maybe.withDefault ""
            , placeholder = Nothing
            , label = labelHidden "Comment"
            }

    else
        text entry.comment


categoryEditAnnotation : Dict Int CatAttempt -> BookEntry -> Element Msg
categoryEditAnnotation categoryEdits entry =
    case Dict.get entry.id categoryEdits of
        Just (Unknown _ error) ->
            row []
                [ warnTriangle [ padding size.xs, Font.color color.red, tooltip below error ] size.l
                , edit [ padding size.xs, Font.color color.darkAccent, tooltip below "Edited entry" ] size.l
                ]

        Just (Known _ _) ->
            row []
                [ checkMark [ padding size.xs, Font.color color.darkAccent ] size.l
                , edit [ padding size.xs, Font.color color.darkAccent, tooltip below "Edited entry" ] size.l
                ]

        Nothing ->
            case entry.categorization of
                None ->
                    Element.none

                _ ->
                    checkMark [ padding size.xs, Font.color color.darkAccent ] size.l


categoryInputText : Dict Int CatAttempt -> BookEntry -> String
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
        [ el [ alignLeft, width fill ] <| text s
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
