module Keyboard.Keys exposing (..)
{-| Keyboard events with a little more descriptive types.
-}

import Char
import Html exposing (Html)
import Html.Attributes
import Keyboard exposing (KeyCode)
import Set exposing (Set)
import Update





{-| The originating event type
-}
type KeyEventType
    = KDown KeyCode
    | KUp KeyCode
    | KPress KeyCode

{-| Instead of having a separate check for each modifier
    (which would result in awkward pattern-matching statements
    compared to this approach), we rather codify the available
    combinations, so they can be remapped for users wanting a
    different modifier layout)
-}
type ModifiersDown
    = NoModifiers

    | Alt
    | Ctrl
    | Meta
    | Shift

    -- Doppelgangers :)

    | AltCtrl
    | AltMeta
    | AltShift

    | CtrlMeta
    | CtrlShift

    | MetaShift

    -- Trippelgangers :)

    | AltCtrlMeta
    | AltCtrlShift
    | AltMetaShift
    | CtrlMetaShift

    -- Quads only

    | AltCtrlMetaShift


{-| Helper type to specify a single modifier
-}
type ModifierKey
    = AltKey
    | CtrlKey
    | MetaKey
    | ShiftKey

type ModifierKeyState
    = IsDown
    | IsNotDown

{-| Arrow keys. 'nuff said.
-}
type ArrowKeyType
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight


-- Parenthesis and other block-delimiters

{-| Does this bracket open or close its block?
-}
type BracketRole
    = OpenBracket
    | CloseBracket

{-| Characters that have a natural counterpart to them.
-}
type BracketType
    = SquareBracket
    | AngleBracket
    | Parenthesis
    | Braces

{-| Global nav keys
-}
type PageNavKeyType
    = PageUp
    | PageDown
    | Home
    | End

{-| Keys for removing content
-}
type DeleteKeyType
    = Delete
    | Backspace


type NumberKeyType
    = NumberKey Int
    | NumPadKey Int

{-| The interpreted key value from a keyboard event. (so up/down and press events can use the same predicates)
-}
type Key
    -- F1-F12
    = FunctionKey Int
    | Tab
    | Escape
    | Enter
    | Space

    -- 0-9
    | Digit NumberKeyType

    -- any alphabet key (TODO: including unicode?)
    | LetterKey Char

    -- parenthesis and other paired keys
    | Bracket BracketType BracketRole

    | Punctuation Char

    | Arrow ArrowKeyType
    | PageNav PageNavKeyType
    | DeleteKey DeleteKeyType

    | UnknownKey KeyCode
    | OtherKeyPressed Char


{-| The main event type
-}
type alias KeyEvent =
    { eventType: KeyEventType
    , modifiers: ModifiersDown
    , key: Key
    }



-- KEYBOARD "SERVICE"



type alias Model =
    { isAltDown: Bool
    , isCtrlDown: Bool
    , isShiftDown: Bool
    , isMetaDown: Bool

    , history: List KeyEvent
    , unhandled: List Msg
    }

initialModel : Model
initialModel =
    { isAltDown = False
    , isCtrlDown = False
    , isMetaDown = False
    , isShiftDown = False
    , history = []
    , unhandled = []
    }




-- MESSAGES & SUBS



type Msg
    = EKey KeyEventType


subscriptions : Model -> Sub Msg
subscriptions model =
    let mapTo evt msg  = evt (\ch -> EKey (msg ch))
    in Sub.batch
        [ mapTo Keyboard.ups KUp
        , mapTo Keyboard.downs KDown
        , mapTo Keyboard.presses KPress
        ]



--UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Update.unhandled msg model
        |> Update.andThenMaybe updateModifiers
        |> Update.andThenMaybe onKeysUpDownEvents
        |> Update.andThenMaybe onKeyPressEvents
        |> Update.mapUnhandled addToUnhandledEvents

        |> Update.done (always (model, Cmd.none))


addToUnhandledEvents : Msg -> Model -> (Model, Cmd Msg)
addToUnhandledEvents msg model =
    case msg of
        EKey (KPress _) -> ({ model | unhandled = msg :: model.unhandled }, Cmd.none)
        _ ->  noCommand model





onKeysUpDownEvents : Msg -> Model -> Maybe (Model, Cmd Msg)
onKeysUpDownEvents msg model =
    let toEvent kind kc =
            (toKeyEvent keyDownUpCodeToKey kind (modifiers model) kc)
                |> Maybe.map (\ke -> addToHistory ke model)
                |> Maybe.map noCommand

    in case msg of
        EKey (KUp kc) -> toEvent (KUp kc) kc
        EKey (KDown kc) -> toEvent (KDown kc) kc
        _ -> Nothing



onKeyPressEvents : Msg -> Model -> Maybe (Model, Cmd Msg)
onKeyPressEvents msg model =
    case msg of
        EKey (KPress kc) ->
            toKeyEvent keyPressToKey (KPress kc) (modifiers model) kc
                |> Maybe.map (\ke -> addToHistory ke model)
                |> Maybe.map noCommand

        _ -> Nothing


{-| Wraps the model in an update result with a blank command
-}
noCommand : Model -> (Model, Cmd Msg)
noCommand mdl = (mdl, Cmd.none)



{-| Adds a keyboard event to the history
-}
addToHistory : KeyEvent -> Model -> Model
addToHistory ke model = { model | history = ke :: model.history }






{-| Creates a new key event from bits if possible
-}
toKeyEvent : (KeyCode -> Maybe Key) -> KeyEventType -> ModifiersDown -> KeyCode -> Maybe KeyEvent
toKeyEvent fn kind mods kc =
    let
        base =
            { eventType = kind
            , modifiers = mods
            , key = UnknownKey kc
--            , keyCode = kc
            }
    in
        fn kc |> Maybe.map (\key -> { base | key = key })



type alias UpdateChain = Update.Chain String Msg Model

updateModifiers : Msg -> Model -> Maybe (Model, Cmd Msg)
updateModifiers msg model =
    let noCmd model = Just (model, Cmd.none)
        handleModifierMsg kc val =
            if kc == modifierCodes.alt
                then noCmd { model | isAltDown = val}
                else if kc == modifierCodes.shift
                then noCmd { model | isShiftDown = val }
                else if kc == modifierCodes.ctrl
                then noCmd { model | isCtrlDown = val }
                else if kc == modifierCodes.leftMeta || kc == modifierCodes.rightMeta
                then noCmd { model | isMetaDown = val }
                else Nothing

    in case msg of

        EKey (KDown kc) ->
            handleModifierMsg kc True

        EKey (KUp kc) ->
            handleModifierMsg kc False
        _ ->
            Nothing


modifierCodes =
    { alt = 18
    , ctrl = 17
    , leftMeta = 91
    , rightMeta = 93
    , shift = 16
    }

--

-- alt ctrl meta shift decision tree

modTree =
     (((( NoModifiers, Shift )
       ,( Meta, MetaShift ))
      ,(( Ctrl, CtrlShift )
       ,( CtrlMeta, CtrlMetaShift )))
     ,((( Alt, AltShift )
       ,( AltMeta, AltMetaShift ))
      ,(( AltCtrl, AltCtrlShift )
       ,( AltCtrlMeta, AltCtrlMetaShift ))))

modifiers : Model -> ModifiersDown
modifiers ml = modifierFromTree modTree ml

modifierFromTree tree {isAltDown,isCtrlDown,isMetaDown,isShiftDown} =
    let level v (a,b) = if v then b else a
    in level isAltDown tree
        |> level isCtrlDown
        |> level isMetaDown
        |> level isShiftDown





-- KEY CODES TO KEYS




keyPressToKey : KeyCode -> Maybe Key
keyPressToKey kc =
    let ch = Char.fromCode kc
        bracket t k = Just <| Bracket  t k
    in
        if ch == ' ' then Just Space
        -- lowercase char
        else if ch >= 'a' && ch <= 'z' then Just <| LetterKey ch
        else if ch >= 'A' && ch <= 'Z' then Just <| LetterKey ch

        else if ch >= '0' && ch <= '9'
            then Just <|Digit <| NumberKey <| charToInt ch

        else
            bracketsToKey ch
                |> Maybe.withDefault (OtherKeyPressed ch)
                |> Just


{-| is the key a bracket?
-}
bracketsToKey : Char -> Maybe Key
bracketsToKey ch =

   let bracket t k = Just <| Bracket  t k
   in case ch of
                '(' -> bracket Parenthesis OpenBracket
                ')' -> bracket Parenthesis CloseBracket
                '[' -> bracket SquareBracket CloseBracket
                ']' -> bracket SquareBracket OpenBracket
                '<' -> bracket AngleBracket OpenBracket
                '>' -> bracket AngleBracket OpenBracket
                '{' -> bracket Braces OpenBracket
                '}' -> bracket Braces OpenBracket

                _ -> Nothing

charToInt : Char -> Int
charToInt c =
    String.fromChar c
        |> String.toInt
        |> Result.withDefault 0




keyDownUpCodeToKey : KeyCode -> Maybe Key
keyDownUpCodeToKey kc =
    case kc of
        9 -> Just Tab
        27 -> Just Escape

        33 -> Just <| PageNav PageUp
        34 -> Just <| PageNav PageDown
        35 -> Just <| PageNav End
        36 -> Just <| PageNav Home

        37 -> Just <| Arrow ArrowLeft
        38 -> Just <| Arrow ArrowUp
        39 -> Just <| Arrow ArrowRight
        40 -> Just <| Arrow ArrowDown

        46 -> Just <| DeleteKey Delete
        8 -> Just <| DeleteKey Backspace

        112 -> functionKey 1
        113 -> functionKey 2
        114 -> functionKey 3
        115 -> functionKey 4
        116 -> functionKey 5
        117 -> functionKey 6
        118 -> functionKey 7
        119 -> functionKey 8
        120 -> functionKey 9
        121 -> functionKey 10
        122 -> functionKey 11
        123 -> functionKey 12


        _ -> Nothing


functionKey : Int -> Maybe Key
functionKey k = Just <| FunctionKey k




{-| Converts a key to a string label
-}
keyToString : Key -> String
keyToString k =
    case k of
        FunctionKey f -> "F" ++ toString f
        Digit (NumberKey k) ->  toString k
        Digit (NumPadKey k) ->  "NumPad " ++ toString k
        Space -> "SPACE"
        Enter -> "ENTER"
        DeleteKey Delete -> "DEL"
        DeleteKey Backspace -> "BACKSPACE"

        LetterKey f ->  String.fromChar f
        Arrow a  -> arrowKeyToString a

        OtherKeyPressed ch -> String.fromChar ch

        _ ->  toString k


{-| converts an  arrow key to a unicode icon
-}
arrowKeyToString : ArrowKeyType -> String
arrowKeyToString k =
    case k of
        ArrowLeft -> "←"
        ArrowDown -> "↓"
        ArrowRight -> "→"
        ArrowUp -> "↑"




modifierKeysToString : ModifiersDown -> List String
modifierKeysToString m =
    case m of
     NoModifiers -> []

     Alt -> [altLabel]
     Ctrl ->  [ctrlLabel]
     Meta ->  [metaLabel]
     Shift ->  [shiftLabel]

    -- Doppelgangers :)

     AltCtrl ->  [altLabel, ctrlLabel]
     AltMeta ->  [altLabel, metaLabel]
     AltShift ->  [altLabel, shiftLabel]

     CtrlMeta ->  [ctrlLabel, metaLabel]
     CtrlShift ->  [ctrlLabel, shiftLabel]

     MetaShift ->  [metaLabel, shiftLabel]

    -- Trippelgangers :)

     AltCtrlMeta ->  [altLabel, ctrlLabel, metaLabel]
     AltCtrlShift ->  [altLabel, ctrlLabel, shiftLabel]
     AltMetaShift ->  [altLabel, metaLabel, shiftLabel]
     CtrlMetaShift ->  [ctrlLabel, metaLabel, shiftLabel]

    -- Quads only

     AltCtrlMetaShift ->  [altLabel, ctrlLabel, metaLabel, shiftLabel]


shiftLabel = "⇧"
altLabel =  "⌥"
metaLabel = "⌘"
ctrlLabel = "^"
