module CAsm.CAsm exposing (..)
{-| Assembly parser and exporter
|-}

import Dict exposing (Dict)
import Html
import CAsm.SymbolType exposing (BitWidth(..), SymbolType(..), typeToString)

type alias SymbolName = String
type alias LabelName = String
type alias FunctionName = String
type alias FunctionArgs = List SymbolName

{-| A symbol is a combination of its name and its type.
|-}
type alias Sym =
    { name: SymbolName
    , type_: SymbolType
    }

{-| Describes the value an entry is assigned to.

|-}
type alias Let =
    { name: SymbolName
    , fn: FunctionName
    , args: FunctionArgs
    }


{-| A block of instructions.
|-}
type alias Blk =
    { name: String
    , phis: List (String, List Let)
    , lets: List Let
    , return: Maybe SymbolName
    }



{-| The type signature of a function.
|-}
type alias FunctionSignature =
    { name: FunctionName
    , argTypes: List SymbolType
    , returnType: SymbolType
    }



type alias CAsm =
    { constants: Dict Int String
    , parameters: List Sym
    , returnType: SymbolType
    , returnsFrom: List LabelName
    , symbols: List Sym
    , calls: List FunctionSignature
    , blocks: List Blk
    }


{-| Returns a new empty block
|-}
emptyBlk : Blk
emptyBlk =
    { name = "main"
    , phis  = [ ]
    , lets = []
    , return = Nothing
    }

{-| Returns a new empty assembly
|-}
empty : CAsm
empty =
    { constants = Dict.empty
    , parameters = []
    , returnType = Void
    , returnsFrom = []
    , symbols = []
    , calls = []
    , blocks = []
    }

-- Helpers for bindings
-- ====================

{-| Adds a new assignment to the head of the block
|-}
addHead : (Blk -> Blk) -> CAsm -> CAsm
addHead mapFn c =
    let
        newBlks = case c.blocks of
            [] -> [ mapFn emptyBlk ]
            x :: xs -> (mapFn x) :: xs
    in
        { c | blocks = newBlks }


{-| Adds a new symbol to the symbol table
|-}
addSymbol : Sym -> CAsm -> CAsm
addSymbol sym c =
    { c | symbols = sym :: c.symbols }


call : Sym -> FunctionName -> List Sym -> FunctionSignature
call assignTo fn args =
    { name = fn
    , argTypes = List.map .type_ args
    , returnType = assignTo.type_
    }

{-| Adds a new call target to the list of CAsm-s called by this the block
|-}
addCall : Sym -> FunctionName -> List Sym -> CAsm -> CAsm
addCall assignTo fn args c =
    { c | calls = (call assignTo fn args) :: c.calls }



-- CASM DSL
-- ========


{-| Adds a constant definition to the assembly
|-}
const : String -> CAsm -> CAsm
const v c =
    let
        append v d = Dict.insert (Dict.size d) v d
    in
        { c | constants = append v c.constants }



{-| Adds a new binding to the current head of the instruction list
|-}
let_ : Sym -> (FunctionName, List Sym) -> CAsm -> CAsm
let_ name (fn,args) c =
    let
        l = Let name.name fn <| List.map .name args

    in
        c
            |> addSymbol name
            |> addCall name fn args
            |> addHead (\b -> { b | lets = l :: b.lets })

{-| Adds a phi-node to the current head of the instruction list
|-}
phi : String -> Sym -> (FunctionName, List Sym) -> CAsm -> CAsm
phi from name (fn,args) c =
    let
        l = Let name.name fn <| List.map .name args

    in
        c
            |> addSymbol name
            |> addCall name fn args
            |> addHead (\b -> { b | phis = (from, [l]) :: b.phis })




block : String -> CAsm -> CAsm
block n c =
    let newBlk = { emptyBlk | name = n }
    in { c | blocks = newBlk :: c.blocks}




prettyPrint : CAsm -> String
prettyPrint c =
    let
        line : String -> String -> List String -> String
        line joiner indent bits =
            indent ++ String.join joiner bits

        l0 = line " " ""
        l1 = line "\t" "\t"
        l2 = line " " "\t\t"


        block : String -> List String -> List String -> List String
        block name params lines =
            case lines of
                [] -> []
                _ -> "" :: (l0 (name :: params)) :: lines

        symBlock : String -> List Sym -> List String
        symBlock name syms =
            block name [] <|
                List.map (\s -> l1 [":" ++ s.name, typeToString s.type_]) syms

        constant k c s = (l1 ["__" ++ toString k, toString c]) :: s
        constants c =
            Dict.foldl constant [] c.constants
                |> block "constants" []

        call {name, argTypes, returnType} =
            l1 <|
                List.append
                    [ typeToString returnType
                    , name
                    ]
                    (List.map typeToString argTypes)

        letLine {name, fn, args} =
            l2 <| List.append [":" ++ name, "=", fn] <| List.map (\a -> ":" ++ a) args

        phiLines (from, lets) =
            "" :: (l1 ["if", toString from]) :: (List.map letLine lets)


        blk { name, lets, phis } =
            block "label" [toString name] <|
                List.concat
                        [ List.concatMap phiLines phis
                        , ["", l1 ["let"]]
                        , List.map letLine lets
                        ]


        s = String.join "\n" <|
            List.concat <|
                List.intersperse [""]
                    [ constants c
                    , symBlock "parameters" c.parameters
                    , symBlock "symbols" c.symbols
                    , block "calls" [] <| List.map call c.calls
                    , List.concatMap blk c.blocks
                    ]
    in
        s


