module CAsm.SymbolType exposing
    ( SymbolType(..)
    , BitWidth(..)
    , ParametricType
    , widthInBits
    , typeToString
    , typeToCCode

    , u64, u32, u16, u8, u1
    , i64, i32, i16, i8, i1
    , char, str
    , bool

    , unwrapPointer
    , concat
    )
{-| Symbol types
|-}

import CAsm.Error as Error exposing (Error)



type alias TypeParameter = SymbolType

type alias ParametricType =
    { name: String
    , args: List TypeParameter
    }
{-
}
    Describes the possible symbol types at low level code.
-}
type SymbolType
    = Signed BitWidth
    | Unsigned BitWidth
    | Structure String
    | Pointer SymbolType
    | Constant SymbolType
    | Parametric ParametricType
    | Void
    | IntegerLiteral
    | FloatLiteral

type BitWidth
    = Bits64
    | Bits32
    | Bits16
    | Bits8
    | Bits1
    | BitsChar
--    | BitsSizeT


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

        BitsChar -> 8

signedIntegralTypeToString : BitWidth -> String
signedIntegralTypeToString w =
    case w of

        Bits1 ->
            "I1"

        Bits8 ->
            "I8"

        Bits16 ->
            "I16"

        Bits32 ->
            "I32"

        Bits64 ->
            "I64"

        BitsChar -> "Char"


unsignedIntegralTypeToString : BitWidth -> String
unsignedIntegralTypeToString w =
    case w of
        Bits1 ->
            "U1"

        Bits8 ->
            "U8"

        Bits16 ->
            "U16"

        Bits32 ->
            "U32"

        Bits64 ->
            "U64"

        BitsChar -> "UChar"

{-| Converts a SymbolType to its CAsm reporesentation
|-}
typeToString : SymbolType -> String
typeToString t =
    case t of
        Signed w ->
            signedIntegralTypeToString w

        Unsigned w ->
            unsignedIntegralTypeToString w

        Structure s ->
            "Struct:" ++ s

        Constant c ->
            "Const:" ++ typeToString c

        Pointer p ->
            "Ptr:" ++ typeToString p

        Parametric t ->
            String.join ":" <|
                t.name :: List.map typeToString t.args

        IntegerLiteral -> "<Type:IntegerLiteral>"
        FloatLiteral -> "<Type:FloatLiteral>"

        Void ->
            "Void"


u64  = Unsigned Bits64
u32  = Unsigned Bits32
u16  = Unsigned Bits16
u8  = Unsigned Bits8
u1  =  Unsigned Bits1

i64  =  Signed Bits64
i32  =  Signed Bits32
i16  =  Signed Bits16
i8  =  Signed Bits8
i1  =  Signed Bits1


bool = i1


char = Signed BitsChar
str = Constant <| Pointer char


{-| Returns true if two types can be considered equal, while considering
free-form constants.
-}
concat : SymbolType -> SymbolType -> Maybe SymbolType
concat a b =
    case (a,b) of
        (Signed aa, Signed bb) -> if aa == bb then Just a else Nothing
        (Unsigned aa, Unsigned bb) -> if aa == bb then Just a else Nothing
        (Signed Bits1, Unsigned Bits1) -> Just bool
        (Signed _, IntegerLiteral) -> Just a
        (Unsigned _, IntegerLiteral) -> Just a
        (IntegerLiteral, Signed _) -> Just b
        (IntegerLiteral, Unsigned _) -> Just b
        (IntegerLiteral, IntegerLiteral) -> Just a
        _ -> Nothing



{-| Returns `Just` the pointed to types for the inner types
-}
unwrapPointer : SymbolType -> Result Error SymbolType
unwrapPointer t =
    case t of
        Pointer inner -> Ok inner
        Constant (Pointer inner) -> Ok inner
        _ -> Err (Error.make "Cannot unwrap non-pointer type")

{-
    TO C TYPES
    ==========
-}


signedIntegralTypeToCCode : BitWidth -> String
signedIntegralTypeToCCode w =
    case w of

        Bits1 ->
            "boolean"

        Bits8 ->
            "int8_t"

        Bits16 ->
            "int16_t"

        Bits32 ->
            "int32_t"

        Bits64 ->
            "int64_t"
        BitsChar -> "char"


unsignedIntegralTypeToCCode : BitWidth -> String
unsignedIntegralTypeToCCode w =
    case w of

        Bits1 ->
            "boolean"

        Bits8 ->
            "uint8_t"

        Bits16 ->
            "uint16_t"

        Bits32 ->
            "uint32_t"

        Bits64 ->
            "uint64_t"

        BitsChar -> "unsigned char"


{-| Returns the C type name for a CAsm type
|-}
typeToCCode : SymbolType -> String
typeToCCode t =
    case t of
        Signed w ->
            signedIntegralTypeToCCode w

        Unsigned w ->
            unsignedIntegralTypeToCCode w

        Structure s ->
            "struct " ++ s

        Constant c ->
            "const " ++ typeToCCode c

        Pointer p ->
            typeToCCode p ++ "*"

        IntegerLiteral -> "<Type:IntegerLiteral>"
        FloatLiteral -> "<Type:FloatLiteral>"

        Void ->
            "void"

        Parametric t ->
           String.join "_" <| (::) t.name <| List.map typeToCCode t.args
