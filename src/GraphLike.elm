{-

GraphLike
-------

<Describe me if possible...>

-}
module GraphLike exposing (..)

import Dict exposing (Dict)
import GraphLike.Types as Types
import GraphLike.EdgeReduce as EdgeReduce
import List.Extra
import Set exposing (Set)





-- import Html exposing (Html)

type alias GraphLike comparable v = Types.GraphLike comparable v
type alias Node comparable v = Types.Node comparable v


node : comparable -> GraphLike comparable v -> Maybe v
node k g =
    Dict.get k g.nodes


-- Semigroup
concat : (comparable -> (comparable,comparable)) -> GraphLike comparable v -> GraphLike comparable v -> GraphLike comparable v
concat keySplitFn a b =
    let
        -- keys found in both GraphLikes
        inBoth = Dict.intersect a.nodes b.nodes |> Dict.map (\k _ -> keySplitFn k)

        renameKey getName k =
            Dict.get k inBoth |> Maybe.map getName |> Maybe.withDefault k



        newA = mapKeys (renameKey Tuple.first) a
        newB = mapKeys (renameKey Tuple.second) b
    in
        { edges = Dict.union newA.edges newB.edges
        , nodes = Dict.union newA.nodes newB.nodes
        }

{-
    Specialized version of concat for stirng-keyed GraphLikes
-}
concatStringKeyed : GraphLike String v -> GraphLike String v -> GraphLike String v
concatStringKeyed = concat (\k -> (k ++ ":a", k ++ ":b"))

-- Monoid
empty : GraphLike k v
empty = { edges = Dict.empty, nodes = Dict.empty }

{-
    Creates a new graphlike from a single key-value pair
-}
single : comparable -> v -> GraphLike comparable v
single k v =
    { edges = Dict.fromList [(k,[])], nodes = Dict.fromList [(k,v)] }




-- Functor
mapNodes : (comparable -> a -> b) -> GraphLike comparable a -> GraphLike comparable b
mapNodes f g =
    { g | nodes = Dict.map f g.nodes }



mapKeys : (comparable0 -> comparable0) -> GraphLike comparable0 a -> GraphLike comparable0 a
mapKeys fn g =
    let
        newNodes = Dict.map (\k v -> (fn k, v)) g.nodes
            |> Dict.values
            |> Dict.fromList

        remapEdges k es s =
            let
                newK = fn k
                renameEdge e = if e == newK then newK else e
            in
                Dict.insert newK (List.map renameEdge es) s

        newEdges = Dict.foldl remapEdges Dict.empty g.edges

    in
        { g
            | edges = newEdges
            , nodes = newNodes
            }




addNode : comparable -> v -> List comparable -> GraphLike comparable v -> GraphLike comparable v
addNode k v edges g =
    {g
        | edges = Dict.insert k edges g.edges
        , nodes = Dict.insert k v g.nodes
        }


-- Apply makes no sense
--apply : GraphLike comparable (a->b) -> GraphLike comparable a -> GraphLike comparable b
--apply gf ga =
--    map


unorderedReduce : (comparable -> v -> List comparable -> s -> s) -> s -> GraphLike comparable v -> s
unorderedReduce f s g =
    let
        edgesFor k =
            Dict.get k g.edges
                |> Maybe.withDefault []
    in
        Dict.foldl
            (\k v s -> f k v (edgesFor k) s)
            s
            g.nodes

