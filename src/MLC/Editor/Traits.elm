module MLC.Editor.Traits exposing (..)
{-| Describe me please...
-}


import MLC.ExpressionCursor exposing (ExpressionCursor)
import MLC.Cursor as Cursor
import MLC.Types as M
import MLC.Editor.State as State exposing (State)
import SEd.Model exposing (Traits)


traits : Traits State ExpressionCursor M.Expression
traits =
    { cursorToStringList = cursorToString
    , nodeToString = nodeToString
    , stateToString = stateToString

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
