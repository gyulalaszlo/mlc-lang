module CAsm.SymbolType exposing
    ( SymbolType(..)
    , BitWidth(..)
    , ParametricType
    , widthInBits
    , typeToString

    , u64, u32, u16, u8, u1
    , i64, i32, i16, i8, i1
    , char, str
    , bool
    )
{-| Symbol types
|-}



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
