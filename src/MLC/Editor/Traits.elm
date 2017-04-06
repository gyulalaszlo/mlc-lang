module MLC.Editor.Traits exposing (..)
{-| Describe me please...
-}


import MLC.ExpressionCursor exposing (ExpressionCursor)
import MLC.Cursor as Cursor
import MLC.Types as M
import MLC.Editor.State as State exposing (State)
import SEd.Model exposing (Traits)
import SEd.NodeView as N exposing (leafMeta, nodeMeta)
import MLC.Editor.StateInList as StateInList
import SEd.CursorView exposing (StackLevel)


traits : Traits State ExpressionCursor M.Expression
traits =
    { cursorToStringList = cursorToString
    , nodeToString = nodeToString
    , stateToString = stateToString

    , toNodeTreeMeta = toNodeViewModel
    , stateMeta = stateStackMeta

    , initialCursor = Cursor.leaf
    , initialData = M.EList []
    , initialState = State.InList StateInList.initialModel
    }


cursorToString : ExpressionCursor -> List String
cursorToString c =
    case c of
        Cursor.Leaf -> ["..."]
        Cursor.Nth k cc -> ("#" ++ toString k) :: cursorToString cc


nodeToString : M.Expression -> String
nodeToString n =
    case n of
        M.EList es ->
            case es of
                [] -> "()"
                x :: _ -> "(" ++ nodeToString x ++ ")"
        M.EKey s -> "key: " ++ toString s


stateToString : State -> String
stateToString s =
    case s of
        State.InList s -> "List"
        State.InKey s -> "Key : " ++ s


-- NODE VIEW


toNodeViewModel : ExpressionCursor -> M.Expression -> N.Model
toNodeViewModel c e =
    toNodeViewModelHelper (Just c) e


toNodeViewModelHelper : Maybe ExpressionCursor -> M.Expression -> N.Model
toNodeViewModelHelper c e =
    let
        isSelected = case c of
            Just Cursor.Leaf -> True
            _ -> False


        nodeSelection = case c of
            Just Cursor.Leaf -> N.IsTarget
            Just (Cursor.Nth _ Cursor.Leaf) -> N.IsInPath
            _ -> N.NotSelected



        cursorFor i child =
            case c of
                Just (Cursor.Nth k ccc) ->
                    if k == i then Just ccc else Nothing
                _ -> Nothing

        convertChild i child =
            toNodeViewModelHelper (cursorFor i child) child

    in
        case e of
            M.EList cs ->
                let meta = { nodeMeta | selection = nodeSelection }
                in N.node meta <| List.indexedMap convertChild cs
            M.EKey s ->
                let meta = { leafMeta | isSelected = isSelected, label = s }
                in N.leaf meta



stateStackMeta : State -> StackLevel
stateStackMeta s =
    case s of
        State.InKey k -> { name = k }
        State.InList s -> StateInList.meta s


