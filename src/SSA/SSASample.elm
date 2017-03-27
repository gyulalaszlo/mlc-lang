{-

   SSASample
   -------

   <Describe me if possible...>

-}


module SSA.SSASample exposing (sample)

import Dict
import GraphLike
import SSA.LabeledList as LabeledList exposing (label)
import SSA.Types exposing (..)
import SSA.SSAForm exposing (..)
import Set



sizeT =
    UnsignedIntegral Bits64


charT =
    ConstType (SignedIntegral Bits8)


strT =
    (PointerType charT)


boolT =
    SignedIntegral Bits1



-- MACROS
-- ======


singleTree : String -> BlockBase -> BlockTree
singleTree l b =
    { head = l, graph = GraphLike.single l b, tails = Set.fromList [l] }




const : SymbolType -> Value -> BlockTree
const t v =
    let n = "const " ++ v
        s = ("value", t)
    in singleTree n { emptyBlockBase | body = [  (s, Constant v ) ], exit = OutReturn s }



call : SymbolName -> List Symbol -> BlockTree
call op syms  =
    let
        names = (List.map Tuple.first syms)
        name = "call " ++ op ++ " with " ++ String.join ", " names
        t = sizeT -- replace me
        inst = (("_", t), FunctionCall op names)
    in
--        FunctionCall op names
        singleTree
            name
            { emptyBlockBase
                | body = [inst]
                , symbols = KeepSymbolScope [] []
                , exit = OutReturn  ("_", t)
                }


expr : Binding  -> BlockTree -> BlockTree
expr (symbol,tree) next =
    let
        open = { emptyBlockBase | symbols = OpenNewWith [] }
        close s = { emptyBlockBase | symbols = OpenNewWith [s] }

        name (n,t) w = ssaTypeToString t ++ ": " ++ n ++ "." ++ w
        n = name symbol

        exprTree =
            concat
--                tree
                (conjTo (n "wrap") (close symbol) tree)
                next

--            GraphLike.addNode (n "close") tree.graph (close symbol) [next.head]
    in
        exprTree
--        { head = tree.head
--        , tails = next.tails
--        , graph =
--            next.graph
----                |> GraphLike.addNode (n "open") open [tree.head]
--                |> GraphLike.addNode (n "close") (close symbol) [next.head]
----                |> GraphLike.concatStringKeyed (rewireExpressionTails (n "close") tree).graph
--        }

return : SymbolType -> InstructionBody -> BlockTree
return t i =
    { head = "return"
    , tails = Set.fromList ["return"]
    , graph =
        GraphLike.single "return"
            { emptyBlockBase
            | exit = OutReturn ("retval", t)
            , body = [ (("retval", t), i) ]
            }
    }


type alias Binding =
    ( Symbol, BlockTree )




-- Instruction generators
-- ======================

conjTo : String -> BlockBase -> BlockTree -> BlockTree
conjTo name newHead children =
    { children
        | head = name
        , graph = GraphLike.addNode name newHead [children.head] children.graph
        }

rewireExpressionTails : String -> BlockTree -> BlockTree
rewireExpressionTails to tree =
    let
        updateTail : String -> Maybe BlockBase -> Maybe BlockBase
        updateTail to =
            Maybe.map <|
                \b -> case b.exit of
                    OutReturn s -> { b | exit = OutLocal, symbols = CloseByExporting [s] }
                    _ -> b

        addEdge : String -> Maybe (List String) -> Maybe (List String)
        addEdge to =
            Maybe.map <|
                \es -> to :: es

        rewireTail : String -> String -> BlockGraph -> BlockGraph
        rewireTail to from graph =
            { graph
                | nodes = Dict.update from (updateTail to) graph.nodes
                , edges = Dict.update from (addEdge to) graph.edges
                }

    in
        { tree | graph = Set.foldl (rewireTail to) tree.graph tree.tails }

concat : BlockTree -> BlockTree -> BlockTree
concat a b =
    { head = a.head
    , tails = b.tails
    , graph = GraphLike.concatStringKeyed (rewireExpressionTails b.head a).graph b.graph

    }

defn : String -> List Symbol -> BlockTree -> BlockTree
defn name args =
    conjTo
        ("public function " ++ name)
        { emptyBlockBase | entry = InPublic, symbols = OpenNewWith args }

let_ : List Binding -> BlockTree -> BlockTree
let_ bs children =
    List.foldr
        expr
        children
        bs
--    conjTo
--        ("let")
--        { emptyBlockBase
--        | symbols = KeepSymbolScope [] (List.map Tuple.first bs)
--        , body = bs
--        }
--        children

-- SAMPLE CODE
-- ===========


lenVar =
    ( "len", sizeT )


iVar =
    ( "i", sizeT )


u64sym n = (n, sizeT)
red = u64sym "red"
green = u64sym "green"
blue = u64sym "blue"

sample : BlockTree
sample =
    (defn "indexOfChar" [ ( "c", charT ), ( "s", strT ) ]
        (let_
            [ (red, (const sizeT "0xff0000"))
            , (green, (const sizeT "0x00ff00"))
            , (blue, (const sizeT "0x0000ff"))

            , (("orange", sizeT), (call "U64.+" [red, green]))
            ]
                (call "U64.+" [red, green, blue])
                ))


--ss =
--    [ "defn"
--    , "indexOfChar"
--    , [ ["c", ["u8"]]
--      , ["s", ["ptr", ["u8"]]]
--      ]
--
--    ,   [ "let", [ ]
--
--        ]
--    ]

--lenCheck : BlocksWithLabel
--lenCheck =
--    blocksFrom
--        [   { label = toString "i < len"
--            , node = SSANode LocalEntry (LocalCall "if")
--            , body = [(("__", boolT), FunctionCall  "U64.lt" ["i", "len"]) ]
--            , symbolsAdded = []
--            , inputs = []
--            , symbols = KeepSymbolScope
--            }]
--
--sample0 : BlocksWithLabel
--sample0 =
--    defn "indexOf" [ ( "c", charT ), ( "s", strT ) ] <|
--        let_ [ ( lenVar, "strlen" ) ] <|
--            loop [ ( iVar, "0" ) ] <|
--                if_
--                    lenCheck
--                    (constant sizeT "0x00")
--                    (constant sizeT "0xffffffff")
--
--
--sample : Blocks
--sample =
--    Maybe.map (\( l, n ) -> n) sample0
--        |> Maybe.withDefault []



