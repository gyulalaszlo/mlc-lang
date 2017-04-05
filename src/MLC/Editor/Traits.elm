module MLC.Editor.Traits exposing (..)
{-| Describe me please...
-}


import MLC.ExpressionCursor exposing (ExpressionCursor)
import MLC.Cursor as Cursor
import MLC.Types as M
import MLC.Editor.State as State exposing (State)
import SEd.Model exposing (Traits)
import SEd.NodeView as N exposing (leafMeta, nodeMeta)


traits : Traits State ExpressionCursor M.Expression
traits =
    { cursorToStringList = cursorToString
    , nodeToString = nodeToString
    , stateToString = stateToString

    , toNodeTreeMeta = toNodeViewModel

    , initialCursor = Cursor.leaf
    , initialData = M.EList []
    , initialState = State.InList
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
        State.InList -> "List"
        State.InKey s -> "Key : " ++ s


-- NODE VIEW


toNodeViewModel : ExpressionCursor -> M.Expression -> N.Model
toNodeViewModel c e =
    toNodeViewModelHelper (Just c) e
--    case e of
--        M.EList cs -> N.node {} <| List.map toNodeViewModel cs
--        M.EKey s -> N.leaf {}

toNodeViewModelHelper : Maybe ExpressionCursor -> M.Expression -> N.Model
toNodeViewModelHelper c e =
    let
        isSelected = case c of
            Just Cursor.Leaf -> True
            _ -> False

        isInPath = case c of
            Just (Cursor.Nth _ Cursor.Leaf) -> True
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
--            Maybe.map (\cc ->
--                case cc of
--                    Cursor.Leaf -> Nothing
--                    Cursor.Nth k ccc ->
--                        if k == i then Just ccc else Nothing

    in
        case e of
            M.EList cs ->
                let meta = { nodeMeta | selection = nodeSelection }
                in N.node meta <| List.indexedMap convertChild cs
            M.EKey s ->
                let meta = { leafMeta | isSelected = isSelected, label = s }
                in N.leaf meta
