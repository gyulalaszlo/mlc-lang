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
import Keyboard.Keys as Keyboard exposing (ArrowKeyType, Key(..))


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
keyEventView { eventType , modifiers , key , keyCode } =
    Html.span
        [ class "key-event-view"
        , class <| case eventType of
            Keyboard.KUp -> "key-event-key-up"
            Keyboard.KDown -> "key-event-key-down"
            Keyboard.KPress -> "key-event-key-press"
        ]
        [ modifiersDown modifiers
        , keyIcon <| keyLabel key
        ]

keyIcons : List String -> Html msg
keyIcons ss = Html.span [ class "key-view-list" ] <| List.map keyIcon ss


keyIcon : String -> Html msg
keyIcon label = Html.span [ class "key-view" ] [ text label ]


modifiersDown : Keyboard.ModifiersDown -> Html msg
modifiersDown m =
    case m of
     Keyboard.NoModifiers -> keyIcons []

     Keyboard.Alt -> keyIcons [altLabel]
     Keyboard.Ctrl -> keyIcons [ctrlLabel]
     Keyboard.Meta -> keyIcons [metaLabel]
     Keyboard.Shift -> keyIcons [shiftLabel]

    -- Doppelgangers :)

     Keyboard.AltCtrl -> keyIcons [altLabel, ctrlLabel]
     Keyboard.AltMeta -> keyIcons [altLabel, metaLabel]
     Keyboard.AltShift -> keyIcons [altLabel, shiftLabel]

     Keyboard.CtrlMeta -> keyIcons [ctrlLabel, metaLabel]
     Keyboard.CtrlShift -> keyIcons [ctrlLabel, shiftLabel]

     Keyboard.MetaShift -> keyIcons [metaLabel, shiftLabel]

    -- Trippelgangers :)

     Keyboard.AltCtrlMeta -> keyIcons [altLabel, ctrlLabel, metaLabel]
     Keyboard.AltCtrlShift -> keyIcons [altLabel, ctrlLabel, shiftLabel]
     Keyboard.AltMetaShift -> keyIcons [altLabel, metaLabel, shiftLabel]
     Keyboard.CtrlMetaShift -> keyIcons [ctrlLabel, metaLabel, shiftLabel]

    -- Quads only

     Keyboard.AltCtrlMetaShift -> keyIcons [altLabel, ctrlLabel, metaLabel, shiftLabel]




keyLabel : Key -> String
keyLabel k =
    case k of
        FunctionKey f -> "F" ++ toString f
        Digit (Keyboard.NumberKey k) ->  toString k
        Digit (Keyboard.NumPadKey k) ->  "NumPad " ++ toString k

        LetterKey f ->  String.fromChar f
        Arrow a  -> arrowKeyToString a

        _ ->  toString k


{-| converts an  arrow key to a unicode icon
-}
arrowKeyToString : ArrowKeyType -> String
arrowKeyToString k =
    case k of
        Keyboard.ArrowLeft -> "←"
        Keyboard.ArrowDown -> "↓"
        Keyboard.ArrowRight -> "→"
        Keyboard.ArrowUp -> "↑"



css : String
css = """

.modifier-keys-down { display:inline-block; }

.key-view { display:inline-block; border:0.1em solid; border-bottom:0.3em solid; border-radius: 0.3em; padding: 0.1em;  }


.key-event-view { display: block; }
.key-event-key-down { }
.key-event-key-up { color: #ccc; }
"""


shiftLabel = "⇧"
altLabel =  "⌥"
metaLabel = "⌘"
ctrlLabel = "^"
