module SSA.Types exposing (..)
{-

SSA.Types
---------

<Describe me if possible...>

-}

import GraphLike.Types exposing (GraphLike, TreeGraph, NodeWithEdges)
import Set exposing (Set)



-- BASICS
-- ======

type alias Name =
    String


type alias Type =
    String


type alias Value =
    String


type alias Operator =
    String


type BitWidth
    = Bits64
    | Bits32
    | Bits16
    | Bits8
    | Bits1
    | Bits0


widthInBits : BitWidth -> Int
widthInBits b =
    case b of
        Bits64 ->
            64

        Bits32 ->
            32

        Bits16 ->
            16

        Bits8 ->
            8

        Bits1 ->
            1

        Bits0 ->
            0

{-
    Describes the possible symbol types at low level code.
-}
type SymbolType
    = SignedIntegral BitWidth
    | UnsignedIntegral BitWidth
    | StructType String
    | PointerType SymbolType
    | ConstType SymbolType
    | Void


type alias SymbolName =
    Name


type alias Symbol =
    ( SymbolName, SymbolType )



{-

## Potential symbol table operations:

- Open a new scope with some symbols
- Close the current scope and export the symbols
- Use the current symbol table

-}

type alias SymbolScopeMap = (SymbolName, Symbol)


type SymbolScope
    = OpenNewWith (List Symbol)
    | CloseByExporting (List Symbol)
    | KeepSymbolScope (List SymbolScopeMap) (List SymbolScopeMap)


-- Labels
-- ======



type alias LabelName =
    String


type alias Label =
    LabelName


type Operation
    = OpBinary
    | OpUnary
    | OpConstant
    | OpFunctionCall


type InstructionBody
    = BinaryOp Operator SymbolName SymbolName
    | FunctionCall SymbolName (List SymbolName)
    | UnaryOp Operator SymbolName
    | Constant Value



type alias Instruction = (Symbol, InstructionBody)
type alias InstructionList = List Instruction

type SSAEdgeType tag
    = Local
    | Public
    | Merge tag
    | Split tag

type alias SSAEdge k tag =
    { from: k
    , to: k
    , kind: SSAEdgeType tag
    }

type alias SSAGraph k tag=
    { edges: List (SSAEdge k tag)
    }




-- SSA Graphs
-- ==========



type EntryKind
    = InMerge
    | InLocal
    | InPublic

type ExitKind
    = OutLocal
    | OutBranch
    | OutReturn Symbol

type alias NodeBase =
    { entry: EntryKind
    , exit:ExitKind
    , edges: List String
    }


type alias BlockGraph = GraphLike String BlockBase
type alias BlockNode = NodeWithEdges String BlockBase

{-
    Bundles a graph and a label for the head node
-}
type alias BlockTree = TreeGraph String BlockBase

type alias BlockBase =
    { body: InstructionList
    , symbols: SymbolScope
    , entry: EntryKind
    , exit: ExitKind
    }


emptyBlockBase : BlockBase
emptyBlockBase = { body = [], symbols = KeepSymbolScope [] [], entry = InLocal, exit = OutLocal }



