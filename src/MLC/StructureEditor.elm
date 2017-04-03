module MLC.StructureEditor exposing (Model, initialModel, Msg(..), subscriptions, update, view)
{-| Describe me please...
|-}

import Char
import List.Extra
import MLC.Cursor as Cursor
import MLC.Types as M
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Keyboard exposing (KeyCode)
import MLC.StateMachine as StateMachine exposing (StateMachine, Transition, isInAnyState, isInState, stateMachine, transition)
import Set


-- MODEL


type Node k v
    = Leaf v
    | Branch (List Node)



type alias Model = StateMachine Msg ModelInner

type alias ModelInner =
    { current: State
    , stack: List State
    , data: M.Expression
    , lastKeys: List Char
    , cursor: Cursor
    }


type State
    = InList
    | InKey String


initialModel : Model
initialModel = stateMachine initialModelInner mlcEditorTransitions


initialModelInner : ModelInner
initialModelInner =
    { current = InList
    , stack = []
    , data = M.EList []
    , lastKeys = []
    , cursor = Cursor.from 0
    }


type alias Cursor = Cursor.Cursor Int


getAtCursor : Int -> M.Expression -> Maybe M.Expression
getAtCursor n e =
    case e of
      M.EList es -> List.Extra.getAt n es
      M.EKey str -> Just e
--      _ -> Nothing

setAtCursor : Int -> M.Expression -> M.Expression -> Maybe M.Expression
setAtCursor n new parent =
    case parent of
        M.EList es ->
            Just <| M.EList <|
                List.concat
                    [ List.take (n) es
                    , [ new ]
                    , List.drop (n + 1) es
                    ]
        M.EKey _ -> Just new

--        _ -> Nothing

insertAtCursor : Int -> M.Expression -> M.Expression -> Maybe M.Expression
insertAtCursor n new parent =
    case parent of
        M.EList es ->
            Just <| M.EList <|
                List.concat
                    [ List.take n es
                    , [ new ]
                    , List.drop n es
                    ]
        M.EKey _ -> Just new

--        _ -> Nothing

getAt : Cursor -> M.Expression -> Maybe M.Expression
getAt c e = Cursor.get getAtCursor c e

setAt : Cursor -> M.Expression -> M.Expression -> Maybe M.Expression
setAt c new parent = Cursor.set getAtCursor setAtCursor c new parent

insertAt : Cursor -> M.Expression -> M.Expression -> Maybe M.Expression
insertAt c new parent = Cursor.set getAtCursor insertAtCursor c new parent
-- MSG




type Msg
    = KeyPress KeyCode
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses KeyPress
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
--    Sub.none



-- UPDATE

keyLimit = 5

update : Msg -> Model -> (Model, Cmd Msg)
update =
    StateMachine.transitionThen <|
        \msg (m, cmd) ->
            case msg of
                KeyPress c -> ({ m | lastKeys = (Char.fromCode c) :: (List.take keyLimit m.lastKeys) }, cmd)
                _ -> (m,cmd)




-- VIEW



view : Model -> Html Msg
view {state} =
    div [ class "StructureEditor-view" ]
        [ text <| toString state
        , Html.p [] [ text <| toString state.data ]
        , exprView state.data
        , lastKeyView state.lastKeys
        , stackView state
        ]


lastKeyView : List Char -> Html Msg
lastKeyView cs =
    case cs of
        [] -> text ""
        x :: xs ->
            div [class "keys-pressed" ]
                [ Html.h1  [ class "last-key" ] [ text <| toString x ]
                ]



stackView : ModelInner -> Html Msg
stackView model =
    Html.ul [] <|
        List.concat
            [ [ Html.li [] [ text <| toString model.current] ]
            , List.map (\v -> Html.li [] [ text <| toString v ]) model.stack
            ]


exprView : M.Expression -> Html msg
exprView e =
    case e of
        M.EList es ->
            Html.span [] <| List.map (Html.li [] << List.singleton << exprView) es
        M.EKey s ->
            Html.span [] [ text s ]
--        v -> Html.span [] [ text <| toString v ]
-- SM 2


-- STATE MACHINE




-- STATE MACHINE STUFF

type alias Trans = Transition Msg ModelInner

keyDownBase : (KeyCode -> Bool -> Bool) -> String -> (Msg -> Bool)
keyDownBase f ks =
    let
        ccs = Set.fromList <| List.map Char.toCode <| String.toList ks
        pred m =
            case m of
                KeyPress kc -> f kc <| Set.member kc ccs
                _ -> False
    in pred


keyDown = keyDownBase (\_ v -> v)
exceptKeys = keyDownBase (\_ v -> not v)


symbolKeys = exceptKeys "(){}[]\"':/ "
nonSymbolKeys = keyDown "(){}[]\"':/ "

inState m = isInState .current
inAnyState = isInAnyState


inList m =
    case m.current of
        InList -> True
        _ -> False

inKey m =
    case m.current of
        InKey _ -> True
        _ -> False

mlcEditorTransitions : List Trans
mlcEditorTransitions =
    [ { on = keyDown "("   , from = inAnyState       , with = startList }
    , { on = keyDown ")"   , from = inList  , with = endList }

    , { on = symbolKeys     , from = inList  , with = startSymbol }
    , { on = symbolKeys     , from = inKey   , with = addSymbolChar }
    , { on = nonSymbolKeys  , from = inKey   , with = endSymbol }
    ]


-- HANDLERS

cursorStepInto : ModelInner -> Cursor -> Cursor
cursorStepInto model c =
    getAt c model.data
        |> Maybe.map (\e ->
            case e of
                M.EList es -> Cursor.push (List.length es) c
                _ -> c )
        |> Maybe.withDefault c

cursorStepOut : Cursor -> Cursor
cursorStepOut c = let cc = Cursor.pop c
    in { cc | head = c.head + 1 }



pushNew : M.Expression -> State -> ModelInner -> (ModelInner, Cmd Msg)
pushNew e state model =
    let c = cursorStepInto model model.cursor
    in
        ({ model
            | current = state
            , stack = model.current :: model.stack
            , cursor = c
            , data = insertAt model.cursor e model.data
                |> Maybe.withDefault (M.EList [])
            }
        , Cmd.none
        )

--updateExpr : M.Expression -> State -> ModelInner -> (ModelInner, Cmd Msg)
--updateExpr e state model =
--    ({ model
--        | current = state
--        , stack = model.current :: model.stack
--        , cursor = cursorStepInto model.cursor
--        , data = setAt model.cursor e model.data |> Maybe.withDefault model.data
--        }
--    , Cmd.none
--    )
--nextElement : ModelInner  -> ModelInner
--nextElement model =
--    { model
--        | data = setAt model.cursor e model.data |> Maybe.withDefault model.data
--        }


popLast : ModelInner -> (ModelInner, Cmd Msg)
popLast model =
    case model.stack of
        [] -> (model, Cmd.none)
        x :: xs ->
            ({ model
                | current = x
                , stack = xs
                , cursor = cursorStepOut model.cursor
                }
            , Cmd.none
            )



startList :  Msg -> ModelInner -> (ModelInner, Cmd Msg)
startList msg model =
    pushNew (M.EList []) InList model
--    ({ state | current = InList, stack = InList :: state.stack }, Cmd.none)



endList :  Msg -> ModelInner -> (ModelInner, Cmd Msg)
endList msg model =
    popLast model
--    case state.stack of
--        [] -> (state, Cmd.none)
--        x :: xs -> ({ state | current = x, stack = xs }, Cmd.none)
--    case state of
--        InList -> (state, Cmd.none)
--        InKey -> (state, Cmd.none)


appendKey : String -> KeyCode -> String
appendKey s kc =
    s  ++ String.fromList [Char.fromCode kc]

keyCode : Msg -> Maybe KeyCode
keyCode m =
    case m of
        KeyPress kc -> Just kc
        _ -> Nothing

startSymbol :  Msg -> ModelInner -> (ModelInner, Cmd Msg)
startSymbol msg model =
    keyCode msg
        |> Maybe.map (\kc ->
            let s = appendKey "" kc
            in pushNew (M.EKey s) (InKey s) model)
        |> Maybe.withDefault (model, Cmd.none)


addSymbolChar :  Msg -> ModelInner -> (ModelInner, Cmd Msg)
addSymbolChar msg model =
--    keyCode msg
--        |> Maybe.map (\kc ->
--            )
    case msg of
        KeyPress kc ->
            case model.current of
                InKey s ->
                    let newS = appendKey s kc
                    in
                        { model
                        | current = InKey newS
                        , data =
                            setAt model.cursor (M.EKey newS) model.data
                                |> Maybe.withDefault model.data
                        } ! []

                _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

endSymbol :  Msg -> ModelInner -> (ModelInner, Cmd Msg)
endSymbol msg model =
    popLast model






-- Keycodes


keyShift = 16
keyAlt = 18
keyCtrl = 17
keyMeta = 91








type EditMode
    = Append
    | Insert
    | Replace

type Position
    = Before
    | After





