module CAsm.DSL exposing (..)
{-| DSL for building Assemblies from Elm for debugging.
|-}

import CAsm.CAsm exposing (..)
import Dict
import List.Extra



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
    if List.any (\s -> s.name == sym.name) c.symbols then c
    else { c | symbols = sym :: c.symbols }


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




block : String -> List (CAsm -> CAsm) -> CAsm -> CAsm
block n fns c =
    let
        buildBody fns c = List.foldr (\f cc -> f cc) c fns

        addBlock bs = {emptyBlk | name = n } :: bs
        addNextBlk c = { c | blocks = addBlock c.blocks}
    in
        buildBody fns <| addNextBlk c


-- EXIT TYPES
-- ==========

return : SymbolName -> CAsm -> CAsm
return s c =
    addHead (\b -> { b | exit = Return s }) c

branch : SymbolName -> LabelName -> LabelName -> CAsm -> CAsm
branch s t f c =
    addHead (\b -> { b | exit = Branch <| BranchExit s t f }) c


cFn : String -> FunctionName
cFn n = FunctionName ExternC n

builtin : String -> String -> FunctionName
builtin p s = FunctionName (Builtin p) s