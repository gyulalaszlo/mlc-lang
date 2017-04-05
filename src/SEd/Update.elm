module SEd.Update exposing (..)
{-| the update method of the Elm architecture spread into bits
-}

import Char
import MLC.StateMachine exposing (StateMachine, transition)
import SEd.Model exposing (Msg(..), Model)
import SEd.CursorView as CursorView
import SEd.UndoStack as UndoStack
import SEd.NodeTree as NodeTree
import Helpers.SplitLayout as SplitLayout
import Update

type alias SMModel x s c n = StateMachine x (Msg s c n) (Model x s c n)


noCmd : model -> (model, Cmd msg)
noCmd model = (model, Cmd.none)

keyLimit = 5

--updateState : (s -> s) -> {m | state:s} -> { m | state:s}
--updateState s m = { m | state = s m.state }

update : Msg s c n -> SMModel x s c n -> (SMModel x s c n, Cmd (Msg s c n))
update msg model =
    Update.unhandled msg model
        |> transition
        |> Update.map (updateState updateLastKeys)
        |> Update.map (updateState updateNodeViewModel)
        |> Update.done (onError model)


{-| Show errors
-}
onError : SMModel x s c n -> x -> (SMModel x s c n, Cmd (Msg s c n))
onError model err =
    let {state} = model
    in { model | state = { state | error = Just err } } ! []



updateChildren : Msg s c n -> Model x s c n -> (Model x s c n, Cmd (Msg s c n))
updateChildren msg model =
    case msg of
        NodeTreeMsg m ->
            let
                (sm, sc) = NodeTree.update m model.nodeTree
            in
                ({ model | nodeTree = sm }, Cmd.map NodeTreeMsg sc)

        CursorViewMsg m -> updateCursorView m model
        UndoStackMsg m -> updateUndoStack m model
        SplitLayoutMsg m -> updateSplitLayout m model


        _ -> (model, Cmd.none)


-- SUB-COMPONENTS


{-| Updates sub component: CursorView
-}
updateCursorView : CursorView.Msg s c -> Model x s c n -> (Model x s c n, Cmd (Msg s c n))
updateCursorView m model =
    let
        (sm, sc) = CursorView.update m model.cursorView
    in
        ({ model | cursorView = sm }, Cmd.map CursorViewMsg sc)




{-| Updates sub component: UndoStack
-}
updateUndoStack : UndoStack.Msg c n -> Model x s c n -> (Model x s c n, Cmd (Msg s c n))
updateUndoStack m model =
    let
        (sm, sc) = UndoStack.update m model.undoStack
    in
        ({ model | undoStack = sm }, Cmd.map UndoStackMsg sc)





updateSplitLayout : SplitLayout.Msg -> Model x s c n -> (Model x s c n, Cmd (Msg s c n))
updateSplitLayout m model =
    let
        (sm, sc) = SplitLayout.update m model.splitLayout
    in
        ({ model | splitLayout = sm }, Cmd.map SplitLayoutMsg sc)



-- LEGACY UPDATE

type alias UpdateFn x s c n = Msg s c n ->  Model x s c n -> (Model x s c n, Cmd (Msg s c n))

{-| Wraps updating the inner state of the state machin
-}
updateState : UpdateFn x s c n -> Msg s c n -> SMModel x s c n -> (SMModel x s c n, Cmd (Msg s c n))
updateState update msg model =
    let (sm,sc) = update msg model.state in { model | state = sm } ! [sc]




updateLastKeys : Msg s c n -> Model x s c n -> (Model x s c n, Cmd (Msg s c n))
updateLastKeys msg m =
    noCmd <|
        case msg of
            KeyPress c ->
                { m | lastKeys = List.take keyLimit <| (Char.fromCode c) ::  m.lastKeys }
            _ -> m


updateNodeViewModel : Msg s c n -> Model x s c n -> (Model x s c n, Cmd (Msg s c n))
updateNodeViewModel msg model =
    ({ model
--    | nodeTree =
--        { nodeView =
--            MLC.NodeViewAdapter.toNodeViewModel
--            model.cursor
--            model.data
--        }
--    , cursorView = CursorView.setCursor (Just model.cursor) model.cursorView
    | cursorView = CursorView.setCursor (Just model.cursor) model.cursorView
    }, Cmd.none)

