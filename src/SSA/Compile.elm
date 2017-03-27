{-

   Compile
   -------

   <Describe me if possible...>

-}


module SSA.Compile exposing (combineLocalCalls)

-- import Html exposing (Html)

import GraphLike exposing (GraphLike, mapNodes)
import GraphLike.EdgeReduce exposing (bulkTransform, filterMapNodes, foldNodes, mergeNodes)
import GraphLike.Types exposing (NodeWithEdges)
import Dict exposing (Dict)
import SSA.SSAForm exposing (..)
import List.Extra exposing (groupWhileTransitively, span, takeWhile)
import SSA.Types exposing (..)
import Set



{-
   Concatenates strings of  local blocks that exit with a LocalCall
-}

type alias ErrorList = List String




--decomposeSSANode : SSANode -> NodeBase
--decomposeSSANode n =
--    let
--        entry = case n.entry of
--            Phi s l -> InMerge
--            LocalEntry -> InLocal
--            PublicEntry -> InPublic
--
--        (exit, edges) = case n.exit of
--            BranchExit t f -> (OutBranch, [t, f])
--            LocalCall n -> (OutLocal, [n])
--            RemoteCall n -> (OutLocal, [n])
--            ReturnCall (n,t) -> (OutReturn, [])
--    in
--        (NodeBase entry exit edges)
--
--
--toGraphLike : Blocks -> BlockGraph
--toGraphLike bs =
--    let
--
--        addBlock b g =
--            let
--                n =
--                    decomposeSSANode b.node
--
--                block =
--                    { body = b.body
--                    , symbols = b.symbols
--                    , entry = n.entry
--                    , exit = n.exit
--                    }
--            in
--                GraphLike.addNode b.label block n.edges g
--    in
--
--        List.foldl addBlock GraphLike.empty bs




{-
    Helper for pairsToChains.
-}
findChain : comparable -> List comparable -> List (comparable, comparable) -> List (comparable, comparable) -> (List comparable, List (comparable, comparable))
findChain r built nonMatch xs =
  case xs of
    [] -> (built, nonMatch)
    (ll,rr) :: rest ->
      if ll == r then findChain rr (built ++ [rr]) nonMatch rest
      else findChain r built ((ll,rr) :: nonMatch) rest


{-

pairsToChains [(1,2),(3,4),(5,6),(4,7),(9,5)]
-- => [[1,2],[9,5,6],[3,4,7]]

-}
pairsToChains : List (comparable,comparable) -> List (List comparable)
pairsToChains a =
  case a of
    [] -> []
    (l,r) :: xs ->
      let (m,nm) = findChain r [l,r] [] xs
      in m :: (pairsToChains nm)




combineList : BlockGraph -> NodeWithEdges String BlockBase -> Maybe (String,String)
combineList g (_, (l,n), outs) =
    let
        combine : String -> BlockBase -> Maybe (String, String)
        combine ol on =
            case (n.exit, on.entry) of
                (OutLocal, InLocal) -> Just (l, ol)
                _ -> Nothing


    in
        -- We can only combine nodes with single in-out connection
        case outs of
            [ol] ->
                GraphLike.node ol g
                    |> Maybe.andThen (combine ol)

            _ -> Nothing



concatBlocks : List BlockBase -> BlockBase
concatBlocks bs =
    case bs of
        [] -> emptyBlockBase
        x :: xs ->
            List.foldl (\i o -> {o | body = o.body ++ i.body, exit = i.exit })  x xs




combineLocalCalls : BlockTree -> BlockTree
combineLocalCalls {head, tails, graph} =
    let
        merged =
            filterMapNodes (combineList graph) graph
                |> pairsToChains

--        -- a set to check which node groups need merging
--        needsMergeSet =
--            List.foldl Set.union Set.empty <| List.map Set.fromList merged
--        -- predicate to filter nodes that need merging
--        needsMerge l v = Set.member l needsMergeSet


        newG labels g =
            case labels of
                [] -> g
                x :: _ -> mergeNodes concatBlocks labels x g

    in
        { head = head
        , tails = tails
        , graph = List.foldl newG graph merged
        }



