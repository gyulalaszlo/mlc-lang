module MLC.ExpressionCursor exposing (..)
{-| Describe me please...
-}

import List.Extra
import MLC.Cursor as Cursor
import MLC.Types as M
import CAsm.Error as Error exposing (Error)

type alias ExpressionCursor = Cursor.Cursor Int


getAtCursor : ExpressionCursor -> M.Expression -> Result Error M.Expression
getAtCursor cursor e =
    case cursor of
        Cursor.Leaf -> Ok e
        Cursor.Nth k _ ->
            case e of
                M.EList es ->
                    List.Extra.getAt k es
                        |> Result.fromMaybe (Error.makeMsg
                            ["ExpressionCursor cannot find the value at"
                            , toString cursor, "inside", toString e
                            ])
                _ ->
                    Err <| Error.makeMsg
                        ["Cannot get Nth of expression:", toString e]



reaplceAt : Int -> a -> List a -> List a
reaplceAt i a l =
    List.concat
        [ List.take (max 0 (i)) l , [ a ] , List.drop (i + 1) l ]


--bumpCursorOnInsert : Cursor.SetPolicy -> ExpressionCursor -> ExpressionCursor
--bumpCursorOnInsert policy c =
--    case (policy, c) of
--        (Cursor.InsertTail, Cursor.Nth k Cursor.Leaf) -> Cursor.Nth (k + 1)


setAtCursor : Cursor.SetPolicy -> M.Expression -> ExpressionCursor -> M.Expression ->  Result Error (ExpressionCursor, M.Expression)
setAtCursor policy new c parent =
    case (policy, parent, c) of

        -- LEAF


        -- Replace leaf is trivial
        (Cursor.Replace, _, Cursor.Leaf) -> Ok (c, new)

        -- KEYS

        -- Keys are always replaced
        (_, M.EKey _, Cursor.Leaf) -> Ok (c,new)
        (_, M.EKey _, Cursor.Nth _ _) ->
            Err (Error.make <| "ExpressionCursor cannot set value inside a leaf")

        -- LISTS

        (Cursor.InsertTail, M.EList es, Cursor.Leaf) ->
            Ok ((Cursor.Nth (List.length es) Cursor.Leaf), M.EList <| es ++ [new])

        (_, M.EList es, Cursor.Nth k cc ) ->
            List.Extra.getAt k es
                |> Result.fromMaybe
                    (Error.makeMsg ["Cannot find index", toString k, "in", toString es])
                |> Result.andThen (setAtCursor policy new cc)
                |> Result.map (\(newC, newV) -> (Cursor.Nth k newC, M.EList <| reaplceAt k newV es ))





isLeaf : M.Expression -> Bool
isLeaf e =
    case e of
        M.EList _ -> False
        _ -> True

cursorTraits : Cursor.CursorTraits Error Int M.Expression
cursorTraits =
    { getter = getAtCursor
    , setter = setAtCursor
    , isLeaf = isLeaf
    , error = Error.make
    }




--step : (Cursor.Direction,Int) -> M.Expression -> M.Expression
--step (direction,n) parent =
--
--    case (direction, e) of
--        (Right, M.EList es) ->
