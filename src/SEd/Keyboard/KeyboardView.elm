module SEd.Keyboard.KeyboardView exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , css
    )
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Keyboard.Keys as Keyboard exposing (ArrowKeyType, Key(..), ModifiersDown(..))


-- MODEL


type alias Model =
    { keyboard: Keyboard.Model
    }


initialModel : Model
initialModel =
    { keyboard = Keyboard.initialModel
    }


-- MSG


type Msg
    = KeyboardMsg Keyboard.Msg




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyboardMsg (Keyboard.subscriptions model.keyboard)



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyboardMsg m -> updateKeyboard m model


{-| Updates sub component: Keyboard
-}
updateKeyboard : Keyboard.Msg -> Model -> (Model, Cmd Msg)
updateKeyboard m model =
    let
        (sm, sc) = Keyboard.update m model.keyboard
    in
        ({ model | keyboard = sm }, Cmd.map KeyboardMsg sc)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "keyboard-view" ]
        [ Html.map KeyboardMsg <| modifierKeysView model.keyboard
        , Html.h4 [] [ text "history:" ]
        , Html.ul [] <| List.map keyEventView model.keyboard.history
        , Html.h4 [] [ text "Unhandled:" ]
        , text <| toString model.keyboard.unhandled
        ]


{-| shows a list of currently pressed modifier keys
-}
modifierKeysView : Keyboard.Model -> Html Keyboard.Msg
modifierKeysView model =
    div [ class "modifier-keys-view" ]
        [ Html.ul [ class "modifier-keys-down" ]
            [ modifierKey "⇧" model.isShiftDown
            , modifierKey "^" model.isCtrlDown
            , modifierKey "⌥" model.isAltDown
            , modifierKey "⌘" model.isMetaDown
            ]
        ]


{-| shows a keyboard icon labeled by label if isDown is True
-}
modifierKey : String -> Bool -> Html msg
modifierKey label isDown =
    case isDown of
        False -> text ""
        True -> Html.li
                    [ class "modifier-key-view key-view" ]
                    [ text label ]





keyEventView : Keyboard.KeyEvent -> Html Msg
keyEventView { eventType , modifiers , key } =
    Html.span
        [ class "key-event-view"
        , class <| case eventType of
            Keyboard.KUp _ -> "key-event-key-up"
            Keyboard.KDown _ -> "key-event-key-down"
            Keyboard.KPress _ -> "key-event-key-press"
        ]
        [ modifiersDown modifiers
        , keyIcon <| Keyboard.keyToString key
        ]

keyIcons : List String -> Html msg
keyIcons ss = Html.span [ class "key-view-list" ] <| List.map keyIcon ss


keyIcon : String -> Html msg
keyIcon label = Html.span [ class "key-view" ] [ text label ]



-- MODIFIER KEYS

modifiersDown : Keyboard.ModifiersDown -> Html msg
modifiersDown m = keyIcons <| Keyboard.modifierKeysToString m







-- CSS


css : String
css = """

.modifier-keys-down { display:inline-block; }

.key-view {  }


.key-event-view { display:inline-block; border:0.1em solid; border-bottom:0.3em solid; border-radius: 0.3em; padding: 0.1em; }
.key-event-key-down { }
.key-event-key-up { color: #ccc; }
"""


