module MLC.StructureEditor exposing (Model, initialModel, Msg(..), subscriptions, update, view)
{-| Describe me please...
|-}

import CAsm.Error as Error exposing (Error)
import Char
import List.Extra
import MLC.Cursor as Cursor
import MLC.ExpressionCursor exposing (ExpressionCursor, cursorTraits)
import MLC.NodeViewAdapter exposing (toNodeViewModel)
import MLC.Types as M
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Keyboard exposing (KeyCode)
import MLC.StateMachine as StateMachine exposing (StateMachine, Transition, isInAnyState, isInState, stateMachine, transition)
import Result.Extra
import SEd.NodeView
import Set


type alias Cursor = ExpressionCursor

-- MODEL


type Node k v
    = Leaf v
    | Branch (List Node)




type alias SM =  StateMachine Error Msg ModelInner
type alias Model = SM
--type alias Model = Result Error SM

type alias ModelInner =
    { current: State
    , stack: List State
    , data: M.Expression
    , lastKeys: List Char
    , cursor: Cursor

    , error: Maybe Error

--    , nodeView: SEd.NodeView.Model
    , nodeView: SEd.NodeView.Model
    }


type State
    = InList
    | InKey String


initialModel : Model
initialModel =  stateMachine initialModelInner mlcEditorTransitions


initialData = M.EList []
initialCursor = Cursor.leaf

initialModelInner : ModelInner
initialModelInner =
    { current = InList
    , stack = []
    , data = initialData
--    , data = sample
    , lastKeys = []
    , cursor = Cursor.leaf

    , error = Nothing
--    , nodeView = SEd.NodeView.initialModel
    , nodeView = toNodeViewModel initialCursor initialData
    }


sample = M.EList
    [ M.EList [ M.EKey "ABC" ]
    , M.EKey "BAR"
    ]

-- MSG




type Msg
    = KeyPress KeyCode
    | KeyDown KeyCode
    | KeyUp KeyCode
    | NodeViewMsg SEd.NodeView.Msg
    | NoOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
--    case model of
--        Err _ ->
--            Sub.none
--
--        Ok _ ->
            Sub.batch
                [ Keyboard.presses KeyPress
                , Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                ]



-- UPDATE

keyLimit = 5

updateState : (s -> s) -> {m | state:s} -> { m | state:s}
updateState s m = { m | state = s m.state }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

    case msg of
        NodeViewMsg m ->
            let
                (sm, sc) = SEd.NodeView.update m model.state.nodeView
            in
                ( updateState (\s -> { s | nodeView = sm }) model
                , Cmd.map NodeViewMsg sc)

        _ ->


            case updateStateMachine msg model of
                Ok (model, cmd) -> (updateState (\s -> { s| error = Nothing}) model, cmd)
                Err err ->
                    ( updateState (\s -> { s | error = Just err }) model
                    , Cmd.none
                    )




updateStateMachine : Msg -> SM -> Result Error (SM, Cmd Msg)
updateStateMachine =
    StateMachine.transitionThen <|
        \msg (m, cmd) ->
            Ok ( updateLastKeys msg <| updateNodeViewModel m , cmd)


updateLastKeys : Msg -> ModelInner -> ModelInner
updateLastKeys msg m =
    case msg of
        KeyPress c ->
            { m | lastKeys = List.take keyLimit <| (Char.fromCode c) ::  m.lastKeys }
        _ -> m


updateNodeViewModel : ModelInner -> ModelInner
updateNodeViewModel sm =
    { sm | nodeView = MLC.NodeViewAdapter.toNodeViewModel sm.cursor sm.data }


-- VIEW



view : Model -> Html Msg
view {state} =
    div [ class "StructureEditor-view" ]
        [ Html.dl []
            [ Html.dt [] [ text "Cursor:"]
            , Html.dd [] [ text <| toString state.cursor ]

            , Html.dt [] [ text "Data:"]
            , Html.dd [] [ text <| toString state.data ]

            , Html.dt [] [ text "stack:"]
            , Html.dd [] [ text <| toString state.stack ]
            ]
        , Html.map NodeViewMsg <| SEd.NodeView.view state.nodeView
--        , exprView state.data
        , lastKeyView state.lastKeys
        , stackView state
        , case state.error of
            Nothing -> text ""
            Just err ->
                div [class "error"]
                    [ Html.pre []
                        [ text <| Error.errorToString err
                        ]
                    ]
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


-- STATE MACHINE




-- STATE MACHINE STUFF

type alias Trans = Transition Error Msg ModelInner

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

cursorStepInto : M.Expression -> Cursor -> Result Error Cursor
cursorStepInto data c =
    Cursor.get cursorTraits c data
        |> Result.map (\e ->
            case e of
                M.EList es -> Cursor.push (List.length es) c
                _ -> c )
        |> Error.wrapErrorMsg ["while cursorStepInto", toString c, "into", toString data  ]




cursorStepOut : M.Expression -> Cursor -> Result Error Cursor
cursorStepOut data c =

    Cursor.get cursorTraits c data
        |> Result.andThen (\e ->
            case e of
                M.EList es -> Cursor.pop cursorTraits c
                _ -> Ok c )
        |> Error.wrapErrorMsg ["while cursorStepOut from", toString c , "in", toString data]


noCmd : model -> (model, Cmd msg)
noCmd model = (model, Cmd.none)




pushNew : M.Expression -> State -> ModelInner -> Result Error (ModelInner, Cmd Msg)
pushNew e state model =
    let

        newCursorAndData model =
            Cursor.set cursorTraits Cursor.InsertTail e model.cursor model.data
                |> Debug.log ("pushNew " ++ toString e ++ " " ++ toString model.cursor ++ "//")
                |> Result.map (\(newCursor, data) -> { model | data = data, cursor = newCursor })
--                |> Result.andThen2 (\ model)
--                |> Result.andThen
--                    (\model ->
--                        cursorStepInto model.data model.cursor
--                            |> Result.map (\c -> { model | cursor = c }))

        updateStackAndState model =
            { model
            | current = state
            , stack = model.current :: model.stack
            }
    in
        (newCursorAndData model)
            |> Result.map updateStackAndState
            |> Result.map noCmd
            |> Error.wrapErrorMsg ["while pushNew " ++ toString e ]




popState : ModelInner -> Result Error ModelInner
popState model =
    case model.stack of
        [] -> Err <| Error.makeMsg ["Cannot pop empty stack from:", toString model.stack]
        x :: xs -> Ok { model | current = x , stack = xs }


-- LIST


startList :  Msg -> ModelInner -> Result Error (ModelInner, Cmd Msg)
startList msg model =
    pushNew (M.EList []) InList model

endList :  Msg -> ModelInner -> Result Error (ModelInner, Cmd Msg)
endList msg model =
    Cursor.pop cursorTraits model.cursor
        |> Result.map (\c -> { model | cursor = c })
        |> Result.map noCmd
        |> Error.wrapErrorMsg [ "in endList" ]
--    popLast model




-- SYMBOLS


appendKey : String -> KeyCode -> String
appendKey s kc =
    s  ++ String.fromList [Char.fromCode kc]

keyCode : Msg -> Maybe KeyCode
keyCode m =
    case m of
        KeyPress kc -> Just kc
        _ -> Nothing




startSymbol :  Msg -> ModelInner -> Result Error (ModelInner, Cmd Msg)
startSymbol msg model =
    case keyCode msg of
        Nothing -> Ok <| noCmd model
        Just kc ->
            let s = appendKey "" kc
            in
                pushNew (M.EKey s) (InKey s) model


addSymbolChar :  Msg -> ModelInner -> Result Error (ModelInner, Cmd Msg)
addSymbolChar msg model =
--    keyCode msg
--        |> Maybe.map (\kc -

    case keyCode msg  of
        Nothing -> Ok <| noCmd model
        Just kc ->
            case model.current of
                InKey s ->
                    let
                        newS = appendKey s kc
                        newData =
                            Cursor.set
                                cursorTraits
                                Cursor.Replace
                                (M.EKey newS)
                                model.cursor
                                model.data
                    in
                        newData
                            |> Result.map (\(newCursor, d) ->
                                    { model | current = InKey newS, data = d, cursor = newCursor})
                            |> Result.map noCmd
                            |> Error.wrapErrorMsg ["while addSymbolChar " ++ toString model.cursor ]


                _ -> Err <| Error.make "In invalid state, not InKey"

endSymbol :  Msg -> ModelInner -> Result Error (ModelInner, Cmd Msg)
endSymbol msg model =
    Cursor.pop cursorTraits model.cursor
        |> Result.map (\c -> { model | cursor = c })
        |> Result.andThen popState
        |> Result.map noCmd
        |> Error.wrapErrorMsg [ "in endList" ]
--    popLast model






-- Key codes


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





