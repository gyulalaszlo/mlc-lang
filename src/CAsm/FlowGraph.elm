module CAsm.FlowGraph exposing (..)
{-| Describe me please...
|-}

import CAsm exposing (Blk, BlkExit(..), CAsm, LabelName, findBy, nextNodeOf)
import List.Extra
import Set exposing (Set)

type FlowNode
    = ReturnLeaf
    | BranchNode (List FlowGraphNode)
    | Step FlowGraphNode
    | LoopMarker
    | ErrorLeaf

type alias FlowGraphNode =
    { name: LabelName
    , path: List LabelName
    , next: FlowNode
    }


flowGraph : CAsm -> Blk -> List LabelName -> FlowGraphNode
flowGraph c b visited =
    let
        getNode n = findBy .name n c.blocks

        recurse n =
            if Set.member n.name (Set.fromList visited)
                then FlowGraphNode n.name visited LoopMarker
                else flowGraph c n (n.name :: visited)
    in
        FlowGraphNode b.name visited <|
            case b.exit of
                Branch {true, false} ->
                    Maybe.map2
                        (\tt ff -> BranchNode [recurse tt, recurse ff])
                        (getNode true)
                        (getNode false)
                        |> Maybe.withDefault ErrorLeaf
                JumpNext ->

                    nextNodeOf c.blocks b
                        |> Maybe.map (\n -> Step <| recurse n)
                        |> Maybe.withDefault ErrorLeaf

                Return _ -> ReturnLeaf


type FlowPath
    = EndsWithReturn (List LabelName)
    | EndsWithLoop (List LabelName) LabelName (List LabelName)

type alias FlowPathList = List FlowPath

split : a -> List a -> (List a, List a)
split v l =
    splitHelper v l []

splitHelper : a -> List a -> List a -> (List a, List a)
splitHelper v l b =
    case l of
        [] -> (b,[])
        x :: xs ->
            if x == v then (b,xs) else splitHelper v xs (b ++ [x])

flowTree : CAsm -> Blk -> List LabelName -> List FlowPath
flowTree c b visited =
    let
        getNode n = findBy .name n c.blocks

        recurse n =
            if Set.member n.name (Set.fromList visited)
                then
                    let (before,after) = split n.name visited
                    in [EndsWithLoop before n.name after]
                else flowTree c n (visited ++ [n.name])
    in
        case b.exit of
            Branch {true, false} ->
                Maybe.map2
                    (\tt ff -> List.concat [recurse tt, recurse ff])
                    (getNode true)
                    (getNode false)
                    |> Maybe.withDefault []
            JumpNext ->
                nextNodeOf c.blocks b
                    |> Maybe.map (\n -> recurse n)
                    |> Maybe.withDefault []

            Return _ -> [ EndsWithReturn (visited) ]

{-| Creates the flow graph for the assembly
|-}
flowGraphFor : CAsm -> (FlowPathList, FlowGraphNode)
flowGraphFor c =
    case c.blocks of
        [] -> ([], { name = "", path = [], next = ErrorLeaf})
        b :: _ -> let v = [b.name] in (flowTree c b v, flowGraph c b v)



loopNodes : List FlowPath  -> List LabelName
loopNodes ps =
    let
        loopNode : FlowPath -> Maybe LabelName
        loopNode n =
            case n of
                EndsWithLoop _ l _ -> Just l
                _ -> Nothing
    in
        List.filterMap loopNode ps



loopEdges : List FlowPath -> List (LabelName, LabelName)
loopEdges ps =
    let
        loopNode n =
            case n of
                EndsWithLoop _ l ns -> Just (List.Extra.last ns |> Maybe.withDefault l , l)
                _ -> Nothing
    in
        List.filterMap loopNode ps



{-
    FLOW CONTROL
    ============

-}

{-| Returns true if the block is a block which should be a loop head.
|-}
isBlockALoop : CAsm -> Blk -> Bool
isBlockALoop c b =
    let
        (flow,_) = flowGraphFor c
        loops = loopNodes flow
    in
        List.member b.name loops

{-| Returns true if the target of a jump is a loop point
|-}
isJumpALoop : CAsm -> LabelName -> LabelName -> Bool
isJumpALoop c from to =
    let
        (flow,_) = flowGraphFor c
        loops = loopEdges flow
    in
        List.member (from,to) loops

{-| Returns true if the block is the target of a jump from a branch
|-}
hasJumpTo: CAsm -> LabelName -> Bool
hasJumpTo c to =
    let
        isJumpTo to b =
            case b.exit of
                Branch b -> (b.true == to || b.false == to)
                JumpNext -> False
                Return _ -> False

    in
        List.any (isJumpTo to) c.blocks
