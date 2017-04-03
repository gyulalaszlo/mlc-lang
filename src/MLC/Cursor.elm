module MLC.Cursor exposing (..)
{-| Describe me please...
|-}

import Html
import List.Extra

type alias Cursor k =
    { path: List k
    , head: k
    }


from : k -> Cursor k
from k =
    { path = []
    , head = k
    }

push : k -> Cursor k -> Cursor k
push k c =
    { c | path = c.head :: c.path, head = k }

pop : Cursor k -> Cursor k
pop c =
    case c.path of
        [] -> c
        h :: t -> {c | path = t, head = h }

--andThen : (List k -> ExprCursor k) -> ExprCursor k -> ExprCursor k


type alias Getter k v = k -> v -> Maybe v
type alias Setter k v = k -> v -> v -> Maybe v


get : Getter k v -> Cursor k -> v -> Maybe v
get f c v =
    let val = f c.head v
    in case c.path of
        [] -> val
        p :: ps ->
            val |> Maybe.andThen (get f (pop c))


set : Getter k v -> Setter k v -> Cursor k -> v -> v -> Maybe v
set getter setter c newValue parent =
    case c.path of
        [] -> setter c.head newValue parent
        p :: ps ->
                getter c.head parent
                    |> Maybe.andThen (set getter setter (pop c) newValue)
                    |> Maybe.andThen (\v -> setter c.head v parent)




type Node
    = Leaf String
    | Branch (List Node)

sample =
    Branch
        [ Leaf "hello"
        , Leaf "world"
        ]

getter : Int -> Node -> Maybe Node
getter i n =
    case n of
        Leaf v -> Nothing
        Branch ns -> List.Extra.getAt i ns

run =
    get getter (push 0 <| from 0) sample

main =
    Html.pre [] [
        Html.text <| toString run
    ]