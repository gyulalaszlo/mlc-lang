module CAsm.CAsm exposing (..)
{-| Assembly parser and exporter
|-}

import Dict exposing (Dict)
import Html
import CAsm.SymbolType exposing (BitWidth(..), SymbolType(..), typeToString)

type alias SymbolName = String
type alias LabelName = String

type PackageName
    = Builtin String
    | ExternC
    | Pkg String

type alias FunctionName =
    { package: PackageName
    , name: String
    }

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

type alias BranchExit =
    { condition: SymbolName
    , true: LabelName
    , false: LabelName
    }

type BlkExit
    = Return SymbolName
    | JumpNext
    | Branch BranchExit

{-| A block of instructions.
|-}
type alias Blk =
    { name: String
    , phis: List (String, List Let)
    , lets: List Let
    , exit: BlkExit
    }



{-| The type signature of a function.
|-}
type alias FunctionSignature =
    { name: FunctionName
    , argTypes: List SymbolType
    , returnType: SymbolType
    }



type alias CAsm =
    { name: String
    , constants: Dict Int String
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
    , exit = JumpNext
    }

{-| Returns a new empty assembly
|-}
empty : CAsm
empty =
    { name = "main"
    , constants = Dict.empty
    , parameters = []
    , returnType = Void
    , returnsFrom = []
    , symbols = []
    , calls = []
    , blocks = []
    }


withNameAndParams : String -> List Sym -> CAsm
withNameAndParams n args =
    { empty | name = n, parameters = args }


prettyPrint : CAsm -> String
prettyPrint c =
    let
        line : String -> String -> List String -> String
        line joiner indent bits =
            indent ++ String.join joiner bits

        l0 = line " " ""
        l1 = line "\t" "\t"
        l2 = line "\t" "\t\t"


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
                    , fnName name
                    ]
                    (List.map typeToString argTypes)

        fnName n = n.name

        letLine {name, fn, args} =
            l2 <| List.append [":" ++ name, fnName fn] <| List.map (\a -> ":" ++ a) args

        letLines lets =
            case lets of
                [] -> []
                _ -> l1 ["let"] :: List.map letLine lets

        phiLine (from, lets) =
            (l1 [toString from]) :: (List.map letLine lets)

        phiLines phis =
            List.concat <| List.intersperse [""] <| List.map phiLine phis


        blkReturn r =
            case r of
                Return s -> [ l1 ["return", ":" ++s]]
                Branch {condition, true, false} -> [ l1 ["branch", ":" ++ condition], l2 [toString true], l2 [toString false]]
                JumpNext -> []

        blk { name, lets, phis, exit } =
--            block "label" [toString name] <|
                List.concat <|
                    List.intersperse [""] <|
                        List.filter (\l -> not <| List.isEmpty l) <|
                            [ ["", l0 ["label", toString name]]
                            , phiLines phis
                            , letLines lets
                            , blkReturn exit
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


