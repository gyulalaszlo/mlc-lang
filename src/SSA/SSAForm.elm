module SSA.SSAForm exposing (..)

import Dict exposing (Dict)
import Codegen.Indented exposing (Line(..), applyIndents, concatLine, concatStatement)
import SSA.LabeledList exposing (LabeledList)
import SSA.Types exposing (..)


--type alias Name = SSA.Types.Name
--type alias Type = SSA.Types.Type
--type alias Value = SSA.Types.Value
--type alias Operator = SSA.Types.Operator
--type alias BitWidth = SSA.Types.BitWidth

--widthInBits : BitWidth -> Int
--widthInBits = SSA.Types.widthInBits
--
--type alias SymbolType = SSA.Types.SymbolType
--type alias SymbolName = SSA.Types.SymbolName
--type alias Symbol = SSA.Types.Symbol


-- Labels
-- ======


--type alias LabelName = SSA.Types.LabelName
--type alias Label = SSA.Types.Label
--type alias Operation = SSA.Types.Operation
--type alias InstructionBody = SSA.Types.InstructionBody
--type alias Instruction = SSA.Types.Instruction
--type alias InstructionList = SSA.Types.InstructionList

--type alias Block =
--    { label : Label
--    , inputs : List Symbol
--    , body : InstructionList
--    , node : SSANode
--    , symbolsAdded : List Symbol
--    , symbols : SymbolScope
--    }
--
--
--type alias Blocks =
--    List Block
--
--
--type alias BlocksWithLabel =
--    LabeledList String Block



--    ( String, Blocks )


--type alias NodeName =
--    String
--
--
--type
--    SSANodeEntry
--    -- A node where the incoming edges are combined
--    -- so the optimizer can combine them with relative
--    -- ease
--    = Phi Symbol (List NodeName)
--    | LocalEntry
--    | PublicEntry
--
--
--type
--    SSANodeExit
--    -- A node where control diverges
--    = BranchExit NodeName NodeName
--      -- call a node in the current SSA scope
--    | LocalCall NodeName
--      -- call outside the current SSA scope
--    | RemoteCall NodeName
--    | ReturnCall Symbol
--
--

-- Potential symbol table operations:

--type alias SymbolScope = SSA.Types.SymbolScope


{-
   A node in the block graph is defined by the entry and exit proprties
-}

--
--type alias SSANode =
--    { entry : SSANodeEntry
--    , exit : SSANodeExit
--    }


signedIntegralTypeToString : BitWidth -> String
signedIntegralTypeToString w =
    case w of
        Bits0 ->
            "void"

        Bits1 ->
            "boolean"

        Bits8 ->
            "char"

        Bits16 ->
            "i16"

        Bits32 ->
            "i32"

        Bits64 ->
            "i64"


unsignedIntegralTypeToString : BitWidth -> String
unsignedIntegralTypeToString w =
    case w of
        Bits0 ->
            "void"

        Bits1 ->
            "u1"

        Bits8 ->
            "u8"

        Bits16 ->
            "u16"

        Bits32 ->
            "u32"

        Bits64 ->
            "u64"


ssaTypeToString : SymbolType -> String
ssaTypeToString t =
    case t of
        SignedIntegral w ->
            signedIntegralTypeToString w

        UnsignedIntegral w ->
            unsignedIntegralTypeToString w

        StructType s ->
            "struct " ++ s

        ConstType c ->
            "const " ++ ssaTypeToString c

        PointerType p ->
            ssaTypeToString p ++ "*"

        Void ->
            "void"


ssaEntryToString : EntryKind -> String
ssaEntryToString e =
    case e of
        InMerge ->
            "phi"

        InLocal ->
            "local"

        InPublic ->
            "public"


--ssaNodeHeaderToString : Label -> EntryKind -> ExitKind -> List Line
--ssaNodeHeaderToString name entry exit =
--    [ Text <| ssaEntryToString entry ++ " " ++ name ]
--
--
--blockHeaderToString : Block -> List Line
--blockHeaderToString { label, node, inputs } =
--    let
--        args =
--            String.join " -> " <| List.map (\( name, t ) -> ssaTypeToString t ++ " " ++ name) inputs
--    in
--        [ Text <| ssaEntryToString node.entry ++ "  " ++ label ++ ": " ++ args ]


ssaInstructionsToCode : InstructionList -> List Line
ssaInstructionsToCode i =
    -- 1. figure out blocks needed (jump chain)
    let
        targetToString ( name, t ) =
            (ssaTypeToString t) ++ " " ++ name

        assignToString s e =
            concatStatement [ targetToString s, " = ", e ]

        instructionToString : Instruction -> List Line
        instructionToString (s,i) =
            case i of
                BinaryOp op l r ->
                    assignToString s (l ++ " " ++ op ++ " " ++ r)

                UnaryOp op l ->
                    assignToString s (op ++ l)

                Constant v ->
                    assignToString s v

                FunctionCall c args ->
                    assignToString s (c ++ "(" ++ String.join ", " args ++ ")")
    in
        List.concatMap instructionToString i


--ssaBlocksToCode : List Block -> List Line
--ssaBlocksToCode =
--    let
--        blockWithLabel b =
--            List.append (blockHeaderToString b) (ssaInstructionsToCode b.body)
--    in
--        List.concatMap blockWithLabel


