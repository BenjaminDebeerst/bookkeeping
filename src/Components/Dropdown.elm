module Components.Dropdown exposing (..)

import Config exposing (color, size)
import Dropdown as D
import Effect exposing (Effect)
import Element exposing (Element, IndexedColumn, el, fill, padding, paddingEach, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


type alias Model item msg =
    { all : List item
    , selected : List item
    , state : D.State item
    , lift : Msg item -> msg
    , config : Config item msg
    }


{-| Wrap type to avoid recursive type alias
-}
type Config item msg
    = Config (D.Config item (Msg item) (Model item msg))


unwrap : Config item msg -> D.Config item (Msg item) (Model item msg)
unwrap (Config config) =
    config


type Msg item
    = SubMsg (D.Msg item)
    | SetSelected (List item)
    | Noop


initMulti :
    { id : String
    , items : List item
    , selected : List item
    , prompt : List item -> String
    , label : item -> String
    , lift : Msg item -> msg
    }
    -> Model item msg
initMulti { id, items, selected, prompt, label, lift } =
    { all = items
    , selected = selected
    , state = D.init id
    , lift = lift
    , config = Config (multiConfig prompt label)
    }


multiConfig : (List item -> String) -> (item -> String) -> D.Config item (Msg item) (Model item msg)
multiConfig prompt label =
    D.multi
        { itemsFromModel = .all
        , selectionFromModel = .selected
        , dropdownMsg = SubMsg
        , onSelectMsg = SetSelected
        , itemsToPrompt = prompt >> text
        , itemToElement = viewItem label
        }
        |> D.withContainerAttributes [ width fill ]
        |> D.withPromptElement (el [ width fill ] (text (prompt [])))
        |> D.withSelectAttributes [ Border.width 1, Border.rounded 5, padding size.s, width fill ]
        |> D.withListAttributes [ width fill, Border.width 1, Border.rounded 5, Background.color color.white ]



-- UPDATE


update : Msg item -> Model item msg -> ( Model item msg, Effect msg )
update msg model =
    case msg of
        SubMsg dMsg ->
            let
                ( state, cmd ) =
                    D.update (unwrap model.config) dMsg model model.state
            in
            ( { model | state = state }, Effect.sendCmd cmd |> Effect.map model.lift )

        SetSelected list ->
            ( { model | selected = list }, Effect.none )

        Noop ->
            ( model, Effect.none )



-- VIEW


view : Model item msg -> Element msg
view model =
    D.view (unwrap model.config) model model.state |> Element.map model.lift


viewItem : (item -> String) -> Bool -> Bool -> item -> Element (Msg item)
viewItem toLabel selected highlighted item =
    el [ padding size.xs ] <|
        Input.checkbox
            []
            { onChange = \_ -> Noop
            , icon = Input.defaultCheckbox
            , checked = selected
            , label =
                Input.labelRight [ paddingEach { top = 0, bottom = 0, right = 0, left = size.s } ] <|
                    text <|
                        toLabel item
            }
