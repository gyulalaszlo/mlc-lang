{-

GraphLikeView
-------

<Describe me if possible...>

-}
module Pages.GraphLikeView exposing (..)

import Dict exposing (Dict)
import GraphLike
import GraphLike.EdgeReduce exposing (foldNodes, foldWalk, mapNodesToList, toNodesWithEdges)
import Html exposing (Html, div, td, th, text)
import Html.Attributes exposing (class, style)
import SSA.Types exposing (BlockBase, BlockGraph, BlockNode, BlockTree)
import SSA.InstructionsTable exposing (codeView, instructionsTable, instructionsToString)
import Set exposing (Set)
import Svg



-- import Html exposing (Html)


graphLikeView : BlockTree -> Html msg
graphLikeView t =
    let


        node k n =
            Html.tr []
                [ th [ class "graph-node-label" ] [text k ]
                , td [ class "graph-node-value" ] [text (toString n)]
                ]

        edges k =
                List.map
                    (\e ->
                        Html.tr []
                            [ td [] [ text k ]
                            , td [] [ text e ]
                        ])

        withEdges = toNodesWithEdges t.graph



--        nodeBox : GraphLike.EdgeReduce.NodeWithEdges comparable v -> s -> s

--        nodeBox (is, (l,n), os) =
--           Html.li [] (nodeBase l n os)

    in
        Html.div []
--            [ Html.ul []
--                <| foldNodes (\n s -> s ++ [ nodeBox n ]) [] g
----                (Dict.values <| Dict.map (\k v -> nodebase k v (Html.li [])  << nodeBase) g.nodes)
--
--
--            , Html.table []
--                [ Html.tbody [] (Dict.map node g.nodes |> Dict.values )
--                ]
--            , Html.table []
--                [ Html.tbody [] (Dict.map edges g.edges |> Dict.values |> List.concat )
--                ]

            [ div [ class "nodes nodes-alt"] <| foldWalk (\n s -> s ++ [nodeBase n]) t.head [] withEdges
--            , div [ class "nodes"] <| mapNodesToList nodeBase t.graph
            ]




type alias NodeBlocksState msg =
    { left: Float
    , top: Float
    , hStep: Float
    , vStep: Float
    , html: List (Html msg)
    }




nodeBase : BlockNode -> Html msg
nodeBase b =
    let
        (ins, (label, node), outs) = b
    in
        div
            [ class "node-base-block" ]
            [ Html.h4 [] [text label]
--            , Html.ul [] <| List.map (\e -> Html.li [] [text e] ) ins
--            , Html.ul [] <| List.map (\e -> Html.li [] [text e] ) outs
    --        , codeView <| instructionsToString b.body
            , instructionsTable b
    --            , Html.p [] [ text <| toString b]
            ]



