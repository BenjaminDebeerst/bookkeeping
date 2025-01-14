module Pages.Settings exposing (Model, Msg, page)

import Components.Tabs as Tabs
import Config exposing (size)
import Effect exposing (Effect)
import Element exposing (Element, el, fill, padding, spacing, width)
import Layouts
import Page exposing (Page)
import Pages.Accounts as Accounts
import Pages.Categories as Categories
import Pages.ImportProfiles as ImportProfiles
import Persistence.Data exposing (Data)
import Route exposing (Route)
import Shared exposing (dataSummary)
import Util.Layout exposing (dataInit, dataUpdate, dataView)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> dataInit shared init (\_ _ -> init)
        , update = dataUpdate shared update
        , subscriptions = \_ -> Sub.none
        , view = dataView shared "Settings" view
        }
        |> Page.withLayout (\_ -> Layouts.Tabs { dataSummary = dataSummary shared })



-- INIT


type alias Model =
    { tab : Tab
    , accountsModel : Accounts.Model
    , categoriesModel : Categories.Model
    , profilesModel : ImportProfiles.Model
    }


type Tab
    = Accounts
    | Categories
    | ImportProfiles


init : Model
init =
    Model
        Accounts
        Accounts.initModel
        (Categories.init () |> Tuple.first)
        (ImportProfiles.init () |> Tuple.first)



-- UPDATE


type Msg
    = TabSelection Tab
    | AccountMsg Accounts.Msg
    | CategoryMsg Categories.Msg
    | ProfileMsg ImportProfiles.Msg


update : Data -> Msg -> Model -> ( Model, Effect Msg )
update data msg model =
    case msg of
        TabSelection tab ->
            ( { model | tab = tab }
            , Effect.none
            )

        AccountMsg subMsg ->
            let
                ( subModel, effect ) =
                    Accounts.update data subMsg model.accountsModel
            in
            ( { model | accountsModel = subModel }, Effect.map AccountMsg effect )

        CategoryMsg subMsg ->
            let
                ( subModel, effect ) =
                    Categories.update data subMsg model.categoriesModel
            in
            ( { model | categoriesModel = subModel }, Effect.map CategoryMsg effect )

        ProfileMsg subMsg ->
            let
                ( subModel, effect ) =
                    ImportProfiles.update data subMsg model.profilesModel
            in
            ( { model | profilesModel = subModel }, Effect.map ProfileMsg effect )



-- VIEW


view : Data -> Model -> Element Msg
view data model =
    el [ spacing size.m, width fill, padding size.m ] <|
        Tabs.tabbedContent
            { allTabs = [ Accounts, Categories, ImportProfiles ]
            , selectedTab = model.tab
            , tabTitles =
                \tab ->
                    case tab of
                        Accounts ->
                            "Accounts"

                        Categories ->
                            "Categories"

                        ImportProfiles ->
                            "Import Profiles"
            , tabIcons = \_ -> Nothing
            , tabMsg = TabSelection
            , content =
                case model.tab of
                    Accounts ->
                        Accounts.view data model.accountsModel |> Element.map AccountMsg

                    Categories ->
                        Categories.view data model.categoriesModel |> Element.map CategoryMsg

                    ImportProfiles ->
                        ImportProfiles.view data model.profilesModel |> Element.map ProfileMsg
            , rightCorner = []
            }
