module CAsm.CPrinter exposing (..)
{-| Describe me please...
|-}


import CAsm.CAsm exposing (..)
import CAsm.SymbolType exposing (BitWidth(..), SymbolType(..), typeToString)
import Codegen.Indented exposing (Line(..), Token, line)
import Dict
import List.Extra


{-| Converts a CAsm assembly to C Code
|-}
toCCode : CAsm -> List Line
toCCode c =
    let
        assignTo sym =
            [ typeToCCode sym.type_ , sym.name ] |> String.join " "
        header =
            List.map assignTo c.parameters
                |> List.intersperse ", "
                |> paren
                |> (::) c.name

        isHead : (a -> Bool) -> List a -> Bool
        isHead pred xs =
            case xs of
                [] -> False
                x :: xs -> pred x

        visibleBlocks bs =
          List.filter (\b -> isHead (\bb ->  bb.name == b.name) bs || (not <| canHoistBlock c b)) bs

    in
        List.concat
            [ [ line header, Text "{", Indent ]
            , [Text "// Symbols",  Empty]
            , symbolsUsed c
            , [ Empty ]
            , List.concatMap (blockToCode c) <| visibleBlocks c.blocks
            , [ Outdent, Text "}" ]
            ]


blockToCode : CAsm -> Blk -> List Line
blockToCode c b =
    (Text <| b.name ++ ":") ::
        List.concat
            [ [ Indent ]
            , blockInner c b
            , [ Outdent, Empty ]
            ]

blockInner : CAsm -> Blk -> List Line
blockInner c b =
        List.concat
            [ lets b.lets
            , exit c b
            ]


nextNodeOf : List Blk -> Blk -> Maybe Blk
nextNodeOf bs b =
    List.Extra.findIndex (\bb -> b == bb) bs
        |> Maybe.andThen (\i -> List.Extra.getAt (i + 1) bs)


exit: CAsm -> Blk -> List Line
exit c b =
    case b.exit of
        Return s -> [ Text <| "return " ++ s ++ ";" ]
        Branch e -> branchExit c b e
        JumpNext -> nextNodeOf c.blocks b
                |> Maybe.map (\next -> hoistPhiLets c b.name next.name)
                |> Maybe.withDefault([Text <|"// CANNOT FIND NEXT BLOCK FOR :" ++ b.name ])

{-| Specialization of exit for branch exits
|-}
branchExit : CAsm -> Blk -> BranchExit -> List Line
branchExit c b e =
    let
        hoists s = hoistPhiLets c b.name s
        goto s = List.concat
            [ hoists s
            , hoistBlocks c s
                -- Goto if we are unable to inline the target
                |> Maybe.withDefault [ line ["goto", s, ";"] ]
            ]
    in ifThenElse e.condition (goto e.true) (goto e.false)



{-| Emits code to update all phi-lets that the result of jumping from `from` to `to` would update.
|-}
hoistPhiLets : CAsm -> LabelName -> LabelName -> List Line
hoistPhiLets c from to =
    findBy .name to c.blocks
        |> Maybe.andThen (\b -> findBy Tuple.first from b.phis)
        |> Maybe.map  (lets << Tuple.second) --(\(_,phis) -> lets phis)
        |> Maybe.withDefault []







canHoistBlock : CAsm -> Blk -> Bool
canHoistBlock c b =
    case (List.length b.phis, b.exit) of
        (0, _) -> True
        (_, Return _) -> True
        _ -> False


hoistBlocks : CAsm -> LabelName -> Maybe (List Line)
hoistBlocks c to =
    findBy .name to c.blocks
        |> Maybe.andThen (\b -> if canHoistBlock c b then Just b else Nothing)
        |> Maybe.map (\b -> blockInner c b)

{-| Emits C code for declaring all symbols that will be used
|-}
symbolsUsed : CAsm -> List Line
symbolsUsed c =

    c.symbols
        |> List.map (\s ->
            [ typeToken <| typeToCCode s.type_
            , symbolToken s.name
            , semiToken
            ])
        |> Table
        |> List.singleton
--    Table <|
--        List.map (\s ->
--            [ typeToken <| typeToCCode s.type_
--            , symbolToken s.name
--            , semiToken
--            ]) c.symbols



{-| Wraps outputting a function call as C code.

Handles builtins and C code.

|-}
functionCall : FunctionName -> FunctionArgs -> List Token
functionCall f a =
    let
        call n =
            List.concat
                [   [ functionNameToken n
                    , parenToken "("
                    ]
                ,   (List.intersperse colonToken <| List.map symbolToken a)
                ,   [ parenToken ")", semiToken ]
                ]
        binary o l r =
               [ symbolToken l
                , opToken o
                , symbolToken r
                , semiToken
                ]

    in
        case f.package of
            Pkg pkg -> call <| pkg ++ "_" ++ f.name
            ExternC -> call <| f.name
            Builtin pkg ->
                case (f.name, a) of
                    ("eq", [l,r]) -> binary "==" l r -- l ++ " == " ++ r ++ ";"
                    ("lt", [l,r]) -> binary "<" l r -- l ++ " < " ++ r ++ ";"
                    ("gt", [l,r]) -> binary ">" l r -- l ++ " > " ++ r ++ ";"
                    ("at", [l,r]) -> binary "[]" l r -- r ++ "[" ++ l ++ "];"

                    ("plus", [l,r]) -> binary "+" l r -- l ++ " + " ++ r ++ ";"
                    _ -> call <| pkg ++ "_" ++ f.name





{-| Generic helper
|-}
findBy : (v -> a) -> a -> List v -> Maybe v
findBy pred val l =
    List.Extra.find (\e -> (pred e) == val) l

{-
    GENERIC STUFF
    =============
-}

symbolToken = Token "symbol"
typeToken = Token "type"
parenToken = Token "parenthesis"
functionNameToken = Token "functionName"
semiToken = Token "semicolon" ";"
colonToken = Token "colon" ","
assignToken = Token "assign" "="
opToken = Token "op"

{-| Outputs a list of let expressions.
|-}
lets : List Let -> List Line
lets ls =
    let aLet l = [ symbolToken l.name, assignToken ] ++  functionCall l.fn l.args
    in List.singleton <| Table <| List.map aLet ls

--{-| Outputs a single let expression
--|-}
--singleLet : Let -> List Line
--singleLet l =
--        [ line [ l.name,  "=", functionCall l.fn l.args ] ]


{-| Models an if-then-else C block.
|-}
ifThenElse : String -> List Line -> List Line -> List Line
ifThenElse cond t f =
    List.concat
        [ [ line ["if", "(", cond, ")", "{"] , Indent ]
        , t
        , [ Outdent , line ["}", "else", "{"], Indent ]
        , f
        , [ Outdent, line ["}"] ]
        ]

{-| Returns a list of arguments for a function call
|-}
letArgs : FunctionArgs -> List String
letArgs a =
    List.intersperse ", " a

paren : List String -> List String
paren s =
    List.concat [ ["("], s, [")"] ]


{-
    TYPES
    =====
-}


signedIntegralTypeToString : BitWidth -> String
signedIntegralTypeToString w =
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


unsignedIntegralTypeToString : BitWidth -> String
unsignedIntegralTypeToString w =
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
            signedIntegralTypeToString w

        Unsigned w ->
            unsignedIntegralTypeToString w

        Structure s ->
            "struct " ++ s

        Constant c ->
            "const " ++ typeToCCode c

        Pointer p ->
            typeToCCode p ++ "*"

        Void ->
            "void"

        Parametric t ->
           String.join "_" <| (::) t.name <| List.map typeToCCode t.args
