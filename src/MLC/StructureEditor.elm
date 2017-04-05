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
import SEd.NodeTree as NodeTree
import SEd.CursorView as CursorView
import SEd.UndoStack as UndoStack
import SEd.Operations exposing (Operation(InsertNodeAt))
import Set
import Update


type alias Cursor = ExpressionCursor

-- MODEL


type Node k v
    = Leaf v
    | Branch (List Node)




type alias SM =  StateMachine Error Msg ModelInner
type alias Model = SM
type alias Operation = SEd.Operations.Operation ExpressionCursor M.Expression
--type alias Model = Result Error SM

type alias ModelInner =
    { current: State
    , stack: List State
    , data: M.Expression
    , lastKeys: List Char
    , cursor: Cursor

    , error: Maybe Error

    -- Sub component: NodeTree
    , nodeTree: NodeTree.Model

    -- Sub component: CursorView
    , cursorView: CursorView.Model Int

    -- Sub component: UndoStack
    , undoStack: UndoStack.Model ExpressionCursor M.Expression





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
     , nodeTree = NodeTree.initialModel
     , cursorView = CursorView.initialModel
     , undoStack = UndoStack.initialModel


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

    | NodeTreeMsg NodeTree.Msg
    | CursorViewMsg CursorView.Msg
    | UndoStackMsg (UndoStack.Msg ExpressionCursor M.Expression)


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




-- GENERIC UPDATE


type alias UpdateResult msg model = (model, Cmd msg)
type alias UpdateFn msg model = msg -> model -> (model, Cmd msg)

andThenUpdate : UpdateFn msg model -> msg -> (model, Cmd msg) -> (model, Cmd msg)
andThenUpdate fn msg (model, cmd) = let (sm, sc) = fn msg model in sm ! [sc, cmd]




-- UPDATE



keyLimit = 5

updateState : (s -> s) -> {m | state:s} -> { m | state:s}
updateState s m = { m | state = s m.state }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Update.unhandled msg model
        |> StateMachine.transition
        |> Update.map (updateInner updateLastKeys)
        |> Update.map (updateInner updateNodeViewModel)
        |> Update.done (\err -> noCmd <| updateState (\s -> {s | error = Just err }) model)


updateChildren : Msg -> ModelInner -> (ModelInner, Cmd Msg)
updateChildren msg model =
    case msg of
        NodeTreeMsg m ->
            let
                (sm, sc) = NodeTree.update m model.nodeTree
            in
                ({ model | nodeTree = sm }, Cmd.map NodeTreeMsg sc)

        CursorViewMsg m ->
            let
                (sm, sc) = CursorView.update m model.cursorView
            in
                ({ model | cursorView = sm }, Cmd.map CursorViewMsg sc)

        UndoStackMsg m ->
            let
                (sm, sc) = UndoStack.update m model.undoStack
            in
                ({ model | undoStack = sm }, Cmd.map UndoStackMsg sc)


        _ -> (model, Cmd.none)


updateInner : UpdateFn Msg ModelInner -> Msg -> Model -> (Model, Cmd Msg)
updateInner update msg model =
    let (sm,sc) = update msg model.state in { model | state = sm } ! [sc]




updateLastKeys : Msg -> ModelInner -> (ModelInner, Cmd Msg)
updateLastKeys msg m =
    noCmd <|
        case msg of
            KeyPress c ->
                { m | lastKeys = List.take keyLimit <| (Char.fromCode c) ::  m.lastKeys }
            _ -> m


updateNodeViewModel : Msg -> ModelInner -> (ModelInner, Cmd Msg)
updateNodeViewModel msg model =
    { model
    | nodeTree =
        { nodeView =
            MLC.NodeViewAdapter.toNodeViewModel
            model.cursor
            model.data
        }
    , cursorView = CursorView.setCursor model.cursor model.cursorView
    } ! []


-- VIEW



view : Model -> Html Msg
view {state} =
    div [ class "StructureEditor-view mkz-view" ]
        [ Html.map CursorViewMsg <| CursorView.view state.cursorView
        , Html.map NodeTreeMsg <| NodeTree.view state.nodeTree
        , Html.map UndoStackMsg <| UndoStack.view state.undoStack
--        , lastKeyView state.lastKeys
--        , stackView state
--        , text <| toString state.history
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

type alias UpdateChain = Update.Chain Error Msg ModelInner

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

mlcEditorTransitions : List (Transition Error Msg ModelInner)
mlcEditorTransitions =
    [ { on = keyDown "("   , from = inAnyState       , with = startList }
    , { on = keyDown ")"   , from = inList  , with = endList }

    , { on = symbolKeys     , from = inList  , with = startSymbol }
    , { on = symbolKeys     , from = inKey   , with = addSymbolChar }
    , { on = nonSymbolKeys  , from = inKey   , with = endSymbol }
    ]


-- HANDLERS

{-| Adds an operation to the undo stack
-}
addOperation : Operation -> ModelInner -> ModelInner
addOperation op model =
    { model | undoStack = UndoStack.push op model.undoStack }



noCmd : model -> (model, Cmd msg)
noCmd model = (model, Cmd.none)




pushNew : M.Expression -> State -> ModelInner -> Result Error (ModelInner, Cmd Msg)
pushNew e state model =
    let

        newCursorAndData model =
            Cursor.set cursorTraits Cursor.InsertTail e model.cursor model.data
                |> Debug.log ("pushNew " ++ toString e ++ " " ++ toString model.cursor ++ "//")
                |> Result.map (\(newCursor, data) -> { model | data = data, cursor = newCursor })

        updateStackAndState model =
            { model
            | current = state
            , stack = model.current :: model.stack
--            , history = (InsertNodeAt model.cursor e) :: model.history
            }
    in
        (newCursorAndData model)
            |> Result.map updateStackAndState
            |> Result.map (\model -> addOperation (InsertNodeAt model.cursor e) model)
            |> Result.map noCmd
            |> Error.wrapErrorMsg ["while pushNew " ++ toString e ]




popState : ModelInner -> Result Error ModelInner
popState model =
    case model.stack of
        [] -> Err <| Error.makeMsg ["Cannot pop empty stack from:", toString model.stack]
        x :: xs -> Ok { model | current = x , stack = xs }


-- LIST


startList :  Msg -> ModelInner -> UpdateChain
startList msg model =
    pushNew (M.EList []) InList model
        |> Update.fromResult msg
--        |> Update.mapHandledModel (\model -> addOperation (InsertNodeAt model.cursor ))


endList :  Msg -> ModelInner -> UpdateChain
endList msg model =
    Cursor.pop cursorTraits model.cursor
        |> Result.map (\c -> { model | cursor = c })
        |> Result.andThen popState
        |> Result.map noCmd
        |> Error.wrapErrorMsg [ "in endList" ]
        |> Update.fromResult msg




-- SYMBOLS


appendKey : String -> KeyCode -> String
appendKey s kc =
    s  ++ String.fromList [Char.fromCode kc]

keyCode : Msg -> Maybe KeyCode
keyCode m =
    case m of
        KeyPress kc -> Just kc
        _ -> Nothing




startSymbol :  Msg -> ModelInner -> UpdateChain
startSymbol msg model =
    case keyCode msg of
        Nothing -> Update.unhandled msg model
        Just kc ->
            let s = appendKey "" kc
            in
                pushNew (M.EKey s) (InKey s) model
                    |> Update.fromResult msg


addSymbolChar :  Msg -> ModelInner -> UpdateChain
addSymbolChar msg model =

    case keyCode msg  of
        Nothing -> Update.unhandled msg model
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
                            |> Update.fromResult msg


                _ -> Update.unhandled msg model



endSymbol :  Msg -> ModelInner -> UpdateChain
endSymbol msg model =
    Cursor.pop cursorTraits model.cursor
        |> Result.map (\c -> { model | cursor = c })
        |> Result.andThen popState
        |> Result.map noCmd
        |> Error.wrapErrorMsg [ "in endList" ]
        |> Update.fromResult msg






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





