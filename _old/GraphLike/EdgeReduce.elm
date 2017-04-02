{-

EdgeReduce
-------

<Describe me if possible...>

-}
module GraphLike.EdgeReduce exposing (

    toNodesWithEdges,
    foldNodes, foldlNodes, foldrNodes,

    mapNodesToList, filterMapNodes,

    bulkTransform, mergeNodes,



    foldWalk
    )

import Dict exposing (Dict)
import GraphLike.Types exposing (GraphLike, Node, NodeWithEdges)
import List.Extra
import Set exposing (Set)



{-
    Returns a dict of label -> (labels of incoming edge sources)
-}
incomingEdges : GraphLike comparable v -> Dict comparable (List comparable)
incomingEdges g =
    let
        baseDict : Dict comparable (List comparable)
        baseDict =
            Dict.map (\_ _ -> []) g.edges

        updateIncomingEdges : comparable -> Maybe (List comparable) -> Maybe (List comparable)
        updateIncomingEdges e =
            Maybe.map ((::) e)

        addIncomingEdge : comparable -> comparable -> Dict comparable (List comparable) -> Dict comparable (List comparable)
        addIncomingEdge k e =
            Dict.update e (updateIncomingEdges k)

        addToIncomingEdges : comparable -> List comparable -> Dict comparable (List comparable) -> Dict comparable (List comparable)
        addToIncomingEdges k v s =
           List.foldl (addIncomingEdge k) s v
    in
        -- All incoming edges
        Dict.foldl addToIncomingEdges baseDict g.edges




toNodesWithEdges : GraphLike comparable v -> Dict comparable (NodeWithEdges comparable v)
toNodesWithEdges g =
    let
        incoming : Dict comparable (List comparable)
        incoming =
            incomingEdges g

        entry : comparable -> Maybe (NodeWithEdges comparable v)
        entry k =
            Maybe.map3
                (\edgesIn node edgesOut -> (edgesIn, (k, node), edgesOut))
                (Dict.get k incoming)
                (Dict.get k g.nodes)
                (Dict.get k g.edges)

        foldEntries : comparable -> Dict comparable (NodeWithEdges comparable v) -> Dict comparable (NodeWithEdges comparable v)
        foldEntries k s =
            entry k
                |> Maybe.map (\v -> Dict.insert k v s)
                |> Maybe.withDefault s


    in
        List.foldl foldEntries Dict.empty <| Dict.keys g.nodes


foldBase folder fn s g =
    let
        ns = toNodesWithEdges g
    in
        folder fn s (Dict.values ns)



foldlNodes : (NodeWithEdges comparable v -> s -> s) -> s -> GraphLike comparable v -> s
foldlNodes = foldBase List.foldl

foldNodes = foldlNodes


foldrNodes : (NodeWithEdges comparable v -> s -> s) -> s -> GraphLike comparable v -> s
foldrNodes = foldBase List.foldr

mapNodesToList : (NodeWithEdges comparable v -> a) -> GraphLike comparable v -> List a
mapNodesToList f g =
    foldrNodes (\n s -> (f n) :: s) [] g

filterMapNodes : (NodeWithEdges comparable v -> Maybe a) -> GraphLike comparable v -> List a
filterMapNodes f g =
    foldrNodes
        (\n s -> case f n of
            Nothing -> s
            Just x -> x ::s)
        [] g



foldWalk : (NodeWithEdges comparable v -> s -> s) -> comparable -> s -> Dict comparable (NodeWithEdges comparable v) -> s
foldWalk fn label s g =
    let
        withEdges = Dict.get label g
        recur label s = foldWalk fn label s g
        outsOf (_,_,o) = o

    in
        withEdges
            |> Maybe.map
                (\n -> List.foldl recur (fn n s) (outsOf n) )
            |> Maybe.withDefault s

{-
    A single node in the transform context
-}
type alias El comparable v =
    { ins: Set comparable
    , label: comparable
    , node: v
    , outs: Set comparable
    }

type alias Elements comparable v =
    List (El comparable v)

{-
    Input for the transform function.

    Ins and outs are the merged input and output labels without the transformed nodes.

-}
type alias TransformIn comparable v =
    { ins: Set comparable
    , outs: Set comparable
    , nodes: Dict comparable v
    }

type alias TransformFn comparable v =
    TransformIn comparable v -> Elements comparable v

{-
    Transforms nodes that fit the predicate in one swoop.
-}
bulkTransform : (TransformFn comparable v) -> (comparable -> v -> Bool) -> GraphLike comparable v -> GraphLike comparable v
bulkTransform fn pred g =
    let
        -- collect incoming edges
        incoming = incomingEdges g
        -- find targets
        nodes = Dict.filter pred g.nodes

        keySet = Set.fromList <| Dict.keys nodes


        unionOf d k s =
            case Dict.get k d of
                Nothing -> s
                Just i -> Set.union (Set.fromList i) s

        foldUnion d =
                (Set.foldl (unionOf d) Set.empty keySet)

        combineKeys d =
            Set.diff (foldUnion d) keySet


        -- new incoming edges are a sum of all edges
        -- minus the merged edges
        newIncoming = combineKeys incoming
        -- new outgoing edges are the sum of all out edges
        -- minus the merged edges
        newOutgoing = combineKeys g.edges

        withoutKeys =
            Set.foldl (\k s -> Dict.remove k s)

        -- remove
        removeIncomingEdge k =
            Maybe.map (\es -> List.filter (\e -> e == k) es)

        withoutElements is d =
            Set.foldl (\k s -> Dict.update k (removeIncomingEdge k)  s ) d is


        newNodes = withoutKeys g.nodes keySet
        newEdges =
                withoutKeys g.edges keySet
                    |> withoutElements newIncoming

        -- call the user-provided stuff after the cleanup
        transformed = fn
            { ins = newIncoming
            , outs = newOutgoing
            , nodes = nodes
            }

        -- insert each result node
        nodesOut =
            List.foldl
                (\{label, node} ns -> Dict.insert label node ns )
                newNodes
                transformed

        -- insert each result edge
        edgesWithOuts =
            List.foldl
                (\{outs, label} ns -> Dict.insert label (Set.toList outs) ns)
                newEdges
                transformed

        addEdge to from d =
            Dict.update from (\i -> Maybe.map (\es -> to :: es) i) d

        edgesWithIns =
            List.foldl
                (\{ins, label} es -> Set.foldl (addEdge label) es ins)
                edgesWithOuts
                transformed



    in
        { g
            | nodes = nodesOut
            , edges = edgesWithIns
            }







mergeNodes : (List v -> v) -> List comparable -> comparable -> GraphLike comparable v -> GraphLike comparable v
mergeNodes valueMapFn labels newLabel g =
    let
        -- new node value is gotten by merging the labeled nodes
        -- using the external function
        newValue =
            valueMapFn <| List.filterMap(\k -> Dict.get k g.nodes) labels

        predSet = Set.fromList labels
        pred k _ = Set.member k predSet

        trf {ins, outs, nodes} =
            [{ label = newLabel
             , node = newValue
             , ins = ins
             , outs = outs
            } ]

--        withoutNodes = removeNodes
    in
        case labels of
            [] -> g
            h :: _ -> bulkTransform trf pred g


