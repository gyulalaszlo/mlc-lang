module MLC.NodeViewAdapter exposing (..)
{-| Describe me please...
|-}

import MLC.Cursor as Cursor
import MLC.Types as M
import MLC.ExpressionCursor exposing (ExpressionCursor)
import SEd.NodeView as N exposing (leafMeta, nodeMeta)


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
