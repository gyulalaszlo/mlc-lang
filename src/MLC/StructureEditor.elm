module MLC.StructureEditor exposing (StateMachineModel, initialStateMachine, subscriptions, view, Msg)
{-| Describe me please...
|-}

import CAsm.Error as Error exposing (Error)
import Char
import List.Extra
import MLC.Cursor as Cursor
import MLC.Editor.State as State exposing (State(..))
import MLC.Editor.Traits
import MLC.ExpressionCursor exposing (ExpressionCursor, cursorTraits)
import MLC.Types as M
import Helpers.SplitLayout as SplitLayout
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Keyboard exposing (KeyCode)
import MLC.StateMachine as StateMachine exposing (StateMachine, Transition, isInAnyState, isInState, stateMachine, transition)
import Result.Extra
import SEd.Model exposing (Model, Msg(..), fromTraits)
import SEd.NodeTree as NodeTree
import SEd.CursorView as CursorView
import SEd.UndoStack as UndoStack
import SEd.Operations exposing (Operation(InsertNodeAt))
import SEd.Update
import Set
import Update


type alias Cursor = ExpressionCursor
type alias Msg = SEd.Model.Msg State ExpressionCursor M.Expression
-- MODEL


type Node k v
    = Leaf v
    | Branch (List Node)



type alias SM =  StateMachine Error Msg Model
type alias StateMachineModel = SM
type alias Operation = SEd.Operations.Operation ExpressionCursor M.Expression

type alias Model = SEd.Model.Model Error State ExpressionCursor M.Expression

initialStateMachine : StateMachineModel
initialStateMachine =  stateMachine (fromTraits MLC.Editor.Traits.traits) mlcEditorTransitions



sample = M.EList
    [ M.EList [ M.EKey "ABC" ]
    , M.EKey "BAR"
    ]

-- MSG


type alias UndoStackMessage = UndoStack.Msg ExpressionCursor M.Expression


-- SUBSCRIPTIONS


subscriptions : StateMachineModel -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses KeyPress
--        , Keyboard.downs KeyDown
--        , Keyboard.ups KeyUp
        ]




-- GENERIC UPDATE


type alias UpdateResult msg model = (model, Cmd msg)
type alias UpdateFn msg model = msg -> model -> (model, Cmd msg)

andThenUpdate : UpdateFn msg model -> msg -> (model, Cmd msg) -> (model, Cmd msg)
andThenUpdate fn msg (model, cmd) = let (sm, sc) = fn msg model in sm ! [sc, cmd]




-- UPDATE


-- VIEW



view : StateMachineModel -> Html Msg
view {state} =
    div [ class "StructureEditor-view mkz-view" ]
        [ Html.map CursorViewMsg <| CursorView.view state.cursorView
        , SplitLayout.view
                state.splitLayout
                [ Html.map NodeTreeMsg <| NodeTree.view state.nodeTree
                , Html.map UndoStackMsg <| UndoStack.view state.undoStack
                ]

        , case state.error of
            Nothing -> text ""
            Just err ->
                div [class "error"]
                    [ Html.pre []
                        [ text <| Error.errorToString err
                        ]
                    ]
        ]




-- STATE MACHINE




-- STATE MACHINE STUFF

type alias UpdateChain = Update.Chain Error Msg Model

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
        InList _ -> True
        _ -> False

inKey m =
    case m.current of
        InKey _ -> True
        _ -> False

mlcEditorTransitions : List (Transition Error Msg Model)
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
addOperation : Operation -> Model -> Model
addOperation op model =
    { model | undoStack = UndoStack.push op model.undoStack }



noCmd : model -> (model, Cmd msg)
noCmd model = (model, Cmd.none)




pushNew : M.Expression -> State -> Model -> Result Error (Model, Cmd Msg)
pushNew e state model =
    let

        newCursorAndData model =
            Cursor.set cursorTraits Cursor.InsertTail e model.cursor model.data
                |> Result.map (\(newCursor, data) -> { model | data = data, cursor = newCursor })

        updateStackAndState model =
            { model
            | current = state
            , stack = model.current :: model.stack
            }
    in
        (newCursorAndData model)
            |> Result.map updateStackAndState
            |> Result.map (\model -> addOperation (InsertNodeAt model.cursor e) model)
            |> Result.map noCmd
            |> Error.wrapErrorMsg ["while pushNew " ++ toString e ]




popState : Model -> Result Error Model
popState model =
    case model.stack of
        [] -> Err <| Error.makeMsg ["Cannot pop empty stack from:", toString model.stack]
        x :: xs -> Ok { model | current = x , stack = xs }


-- LIST


startList :  Msg -> Model -> UpdateChain
startList msg model =
    pushNew (M.EList []) State.inList model
        |> Update.fromResult msg


endList :  Msg -> Model -> UpdateChain
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




startSymbol :  Msg -> Model -> UpdateChain
startSymbol msg model =
    case keyCode msg of
        Nothing -> Update.unhandled msg model
        Just kc ->
            let s = appendKey "" kc
            in
                pushNew (M.EKey s) (InKey s) model
                    |> Update.fromResult msg


addSymbolChar :  Msg -> Model -> UpdateChain
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



endSymbol :  Msg -> Model -> UpdateChain
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


