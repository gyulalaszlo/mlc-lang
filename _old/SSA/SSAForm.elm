module SSA.SSAForm exposing (..)

import Dict exposing (Dict)
import Codegen.Indented exposing (Line(..), applyIndents, concatLine, concatStatement)
import SSA.LabeledList exposing (LabeledList)
import SSA.Types exposing (..)


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


