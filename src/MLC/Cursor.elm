module MLC.Cursor exposing (..)
{-| Describe me please...
|-}

import Html
import List.Extra
import Result.Extra


type Cursor k
    = Leaf
    | Nth k (Cursor k)
--    | Tail


leaf : Cursor k
leaf = Leaf

branch : k -> Cursor k
branch k = Nth k Leaf





push : k -> Cursor k -> Cursor k
push k c = Debug.log "Cursor.push:" <| Nth k c


pop : CursorTraits x k v -> Cursor k -> Result x (Cursor k)
pop traits c =
    Debug.log ("Cursor.pop:"  ++ toString c) <|
        case c of
            Nth _ cc -> popHelper traits c
            _ -> Err <| traits.error ("Cannot pop cursor :" ++ toString c)

popHelper : CursorTraits x k v -> Cursor k -> Result x (Cursor k)
popHelper traits c =
    case c of
        Nth _  Leaf -> Ok Leaf
        Nth k cc -> popHelper traits cc |> Result.map (Nth k)
        _ -> Err <| traits.error "pop() should never reach this fallback"



type SetPolicy
    = Replace
    | InsertTail



type alias ErrorFactory x = (String -> x)
type alias Getter x k v = Cursor k -> v -> Result x v
type alias Setter x k v = SetPolicy -> v -> Cursor k -> v -> Result x (Cursor k, v)
type alias IsLeaf v = v -> Bool


type alias CursorTraits x k v =
    { getter: Getter x k v
    , setter: Setter x k v
    , isLeaf: IsLeaf v
    , error: ErrorFactory x
    }





recursiveGet : (Maybe k -> v -> Result x v) -> CursorTraits x k v -> Cursor k -> v -> Result x v
recursiveGet f traits c v =
    case c of
        Leaf -> Ok v
--        Tail -> Ok v
        Nth k cc ->
            if traits.isLeaf v then
                Err <| traits.error <|
                    "Cannot step into non-leaf node: "
                    ++ toString v ++ " with Path: " ++ toString c
            else
                (traits.getter c v)
                    |> Result.andThen (\vv -> recursiveGet f traits cc vv)



get : CursorTraits x k v -> Cursor k -> v -> Result x v
get traits c v =
    recursiveGet (\_ v -> Ok v) traits c v



--recursiveSet : CursorTraits x k v -> SetPolicy -> v -> Cursor k -> v -> Result x v
--recursiveSet traits policy v c vv =
--    case c of
--        Leaf -> Ok v
----        Tail -> traits.setter policy v c vv
--        Nth k cc ->
--            if traits.isLeaf vv then
--                Err <| traits.error <|
--                    "Cannot step into non-leaf node: "
--                    ++ toString vv ++ " with Path: " ++ toString c
--            else
--                (traits.getter c vv)
--                    |> Result.andThen (\oldV -> recursiveSet traits policy v cc oldV)
--                    |> Result.andThen (\newV -> traits.setter policy newV c vv)

set : CursorTraits x k v -> SetPolicy -> v -> Cursor k -> v -> Result x (Cursor k, v)
set traits = traits.setter



type Direction
    = Up
    | Down
    | Left
    | Right

type Mode
    = InsertMode
    | ReplaceMode
    | NormalMode

type alias Step = (Direction, Int)

type alias Directed v =
    { up: v
    , down: v
    , left: v
    , right: v
    }

type alias MapToDirections a b = a -> Directed b
type alias CanMove a = a -> Directed Bool
type alias FoldDirection a = Step -> a -> a