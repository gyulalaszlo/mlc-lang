module CAsm.CPrinter exposing (..)
{-| Describe me please...
|-}


import CAsm.CAsm exposing (..)
import CAsm.FlowGraph exposing (FlowPath(..), flowGraphFor, hasJumpTo, isBlockALoop, isJumpALoop, loopEdges, loopNodes)
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
            , [ Empty ]
            , flowPathsAsComment c
            , [ Empty ]
            , symbolsUsed c
            , [ Empty ]
            , List.concatMap (blockToCode c) <| visibleBlocks c.blocks
            , [ Outdent, Text "}" ]
            ]


{-| Converts a single block to C code with labels, indentation and instructions
|-}
blockToCode : CAsm -> Blk -> List Line
blockToCode c b =
    blockWrapper c b <| blockInner c "" b


{-| Outputs the correct header for a block (label, loop or nothing) and wraps the contents
potentially in braces
|-}
blockWrapper : CAsm -> Blk -> List Line -> List Line
blockWrapper c b lines =
    let
        withEmptyHead ls = Empty :: (List.concat ls)
    in
        withEmptyHead <|
            if isBlockALoop c b then
                [[ -- labelComment b.name
--                 , loopHead c b
--                 , Text <| "while (true) {"
                 --, Indent
                 ]
                , lines
--                , [Outdent, Text "}"]
                ]
            else
                if hasJumpTo c b.name then
                    [[ Text <| b.name ++ ":", Indent], lines, [ Outdent]]
                else
                    [ --[labelComment b.name]
                    -- ,
                    lines
                    ]

labelComment : LabelName -> Line
labelComment l = Text  <| "// " ++ l ++ ":"

loopHead : CAsm -> LabelName -> Blk -> List Line
loopHead c from b =
    let
        while cond =
            Table <|
                [[keywordToken "while", parenToken "("] ++ cond ++ [ parenToken ")", parenToken "{"]]


    in
        case b.exit of
            Branch {condition, true, false} ->
                List.concat <|
                    [ [ (while <| inlineArgs c from condition), Indent]
                    , hoisted c b.name true
                    , [ Outdent, Text "}" ]
                    , hoisted c b.name false
                    ]
            _ -> [ while [ keywordToken "true" ] ]


blockInnerWrapper : CAsm -> LabelName -> Blk -> List Line -> List Line
blockInnerWrapper c l b inner =
    let
        isLoop = isBlockALoop c b

    in
        if isLoop then List.append (loopHead c l b) inner
        else List.append inner (exit c l b)
--        case b.exit of
--            Branch {condition} -> while <| inlineArgs c from condition
--            _ -> while [ keywordToken "true" ]


{-| Shared functionality for the actual instructions in the block sans the label and the indentation.
|-}
blockInner : CAsm -> LabelName -> Blk -> List Line
blockInner c from b =
    blockInnerWrapper c from b <|
        List.concat <|
            [ --[ labelComment b.name ]
            --,
            lets c from <| List.filter (\l -> valueKind c l.name == LValue) b.lets
--            , exit c from b
            ]


{-| Generates the exit part of the block
|-}
exit: CAsm -> LabelName -> Blk -> List Line
exit c from b =
    case b.exit of
        Return s -> [ Table <| [[ keywordToken "return " ] ++ inlineArgs c from s  ++ [ semiToken ] ]]
        Branch e -> branchExit c b e
        JumpNext -> nextNodeOf c.blocks b
                |> Maybe.map (\next -> hoistPhiLets c b.name next.name)
                |> Maybe.withDefault([Text <|"// CANNOT FIND NEXT BLOCK FOR :" ++ b.name ])

{-| Specialization of exit for branch exits
|-}
branchExit : CAsm -> Blk -> BranchExit -> List Line
branchExit c b e =
    let
        goto = hoisted c b.name
--        hoists s = hoistPhiLets c b.name s
--        goto s = List.concat
--            [ hoists s
--            , hoistBlocks c b.name s
--                -- Goto if we are unable to inline the target
--                |> Maybe.withDefault (gotoOrContinue c b.name s)
--            ]
    in ifThenElse (inlineArgs c b.name e.condition) (goto e.true) (goto e.false)


hoisted : CAsm -> LabelName -> LabelName -> List Line
hoisted c from to =
    let
        hoists to = hoistPhiLets c from to
        goto to = List.concat
            [ hoists to
            , hoistBlocks c from to
                -- Goto if we are unable to inline the target
                |> Maybe.withDefault (gotoOrContinue c from to)
            ]
    in
        goto to --ifThenElse (inlineArgs c b.name e.condition) (goto e.true) (goto e.false)



gotoOrContinue : CAsm -> LabelName -> LabelName -> List Line
gotoOrContinue c from to =
    if isJumpALoop c from to then [ Text "continue;" ] else [ Text <| "goto " ++ to ++ ";"  ++ toString (from,to)]



{-| Emits code to update all phi-lets that the result of jumping from `from` to `to` would update.
|-}
hoistPhiLets : CAsm -> LabelName -> LabelName -> List Line
hoistPhiLets c from to =
    findBy .name to c.blocks
        |> Maybe.andThen (\b -> findBy Tuple.first from b.phis)
        |> Maybe.map (\(_,phis) -> lets c from (List.filter (\p -> isLValue c p.name) phis))
        |> Maybe.withDefault []






{-| Returns true if a block can be inlined into its caller.
|-}
canHoistBlock : CAsm -> Blk -> Bool
canHoistBlock c b =
    case (List.length b.phis, b.exit) of
        (0, _) -> True
        (_, Return _) -> True
        _ -> False

{-| Tries to inline a call to the `to` label
|-}
hoistBlocks : CAsm -> LabelName -> LabelName -> Maybe (List Line)
hoistBlocks c from to =
    findBy .name to c.blocks
        |> Maybe.andThen (\b -> if canHoistBlock c b then Just b else Nothing)
        |> Maybe.map (\b -> blockInner c from  b)

{-| Emits C code for declaring all symbols that will be used
|-}
symbolsUsed : CAsm -> List Line
symbolsUsed c =
    let
        refCount s = "Refs: " ++ toString (symbolUseCount c s.name)
        rval s = toString (valueKind c s.name)
    in
        c.symbols
            |> List.filter (\{valueKind} -> valueKind == LValue)
            |> List.map (\s ->
                [ typeToken <| typeToCCode s.type_
                , symbolToken s.name
                , semiToken
                , commentToken <| "//"
                , commentToken <| refCount s
                , commentToken <| rval s
                ])
            |> Table
            |> List.singleton

{-| Wraps outputting a function call as C code.

Handles builtins and C code.

|-}
functionCall : CAsm -> LabelName -> FunctionName -> FunctionArgs -> List Token
functionCall c label f a =
    let
        call n =
            List.concat
                [   [ functionNameToken n
                    , parenToken "("
                    ]
--                , inlineArgs c a
                ,   (List.concat <| List.intersperse [ colonToken ] <| List.map (inlineArgs c label) a)
                ,   [ parenToken ")" ]
                ]
        binary o l r =
            List.concat
                [ inlineArgs c label l
                , [ opToken o]
                , inlineArgs c label r
                ]
--               [ symbolToken l
--                , opToken o
--                , symbolToken r
--                ]

    in
        case f.package of
            Pkg pkg -> call <| pkg ++ "_" ++ f.name
            ExternC -> call <| f.name
            Builtin pkg ->
                case (f.name, a) of
                    ("eq", [l,r]) -> binary "==" l r
                    ("lt", [l,r]) -> binary "<" l r
                    ("gt", [l,r]) -> binary ">" l r
                    ("at", [l,r]) ->
                        List.concat
                            [ inlineArgs c label l
                            , [parenToken "["]
                            , inlineArgs c label r
                            , [parenToken "]"]
                            ]

                    ("plus", [l,r]) -> binary "+" l r
                    ("alias", [r]) -> inlineArgs c label r
                    ("from-const", [r]) -> [ symbolToken r]
                    _ -> call <| pkg ++ "_" ++ f.name



isAConditionalLoop : CAsm -> Blk -> Bool
isAConditionalLoop c b =
    case b.exit of
        Branch _ ->
            if List.isEmpty <| List.filter (\l -> isLValue c l.name) b.lets
                then True
                else False
        _ -> False


{-| Returns the number of times a symbol is referenced (stands on the right side as argument).
If only one, then this symbol can be inlined, as its only an rvalue.
|-}
symbolUseCount : CAsm -> SymbolName -> Int
symbolUseCount c n =
    let
        useCountFolder : Let -> Int
        useCountFolder l =
            if List.member n l.args then 1 else 0

        useCountIn : List Let -> Int
        useCountIn ls =
            List.map useCountFolder ls
                |> List.foldl (+) 0

        -- sums usage in all phis (as they all need to be instantiated at some point)
        useInPhis phis =
            List.map (\(_,ls) -> useCountIn ls) phis

        useInExit b =
            case b.exit of
                Return sym -> if n == sym then 1 else 0
                Branch {condition} -> if n == condition then 1 else 0
                _ -> 0

        sumForBlock b memo =
            memo
            + useCountIn b.lets
            + (List.sum <| useInPhis b.phis)
            + (useInExit b)

    in
        List.foldl sumForBlock 0 c.blocks


valueKind : CAsm -> SymbolName -> SymbolValueKind
valueKind c n =
    findBy .name n c.symbols
        |> Maybe.map .valueKind
        |> Maybe.withDefault LValue

isLValue : CAsm -> SymbolName -> Bool
isLValue c n = valueKind c n == LValue

isRValue : CAsm -> SymbolName -> Bool
isRValue c n = valueKind c n == RValue


{-| Returns the flow paths through a function as comments in the C code
|-}
flowPathsAsComment : CAsm -> List Line
flowPathsAsComment c =
    let
        (flow, _) = flowGraphFor c
        path p = String.join " -> " p
        flowLine f =
            case f of
                EndsWithReturn p ->
                    Text <| "// RETURN: " ++ (path p)
                EndsWithLoop b n a ->
                    Text <| "// LOOP: " ++ (path b) ++ " -> (" ++ n ++ " -> " ++ (path a) ++ " -> ...)"
    in
        List.map flowLine flow

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
commentToken = Token "comment"
keywordToken = Token "keyword"

{-| Outputs a list of let expressions.
|-}
lets : CAsm -> LabelName -> List Let -> List Line
lets c ln ls =
    let
        aLet l =
            case valueKind c l.name of
                LValue -> letLValue c ln l
                RValue -> letRValue c ln l
    in
        List.singleton <| Table <| List.map aLet ls


letLValue : CAsm -> LabelName -> Let -> List Token
letLValue c ln l =
    List.concat
        [ [ symbolToken l.name, assignToken ]
        , functionCall c ln l.fn l.args
        ]

letRValue : CAsm -> LabelName -> Let -> List Token
letRValue c ln l =
    [  symbolToken l.name, assignToken ] ++ functionCall c ln l.fn l.args


{-| Tries to inline the arguments for a function call recursively until it hits an lvalue
|-}
inlineArgs : CAsm -> LabelName -> SymbolName -> List Token
inlineArgs c l n =
    case valueKind c n of
        LValue -> [symbolToken n]
        RValue ->
            findSymbolDefinition n l c
                |> List.concatMap (\{fn, args} -> functionCall c l fn args )




{-| Models an if-then-else C block.
|-}
ifThenElse : List Token -> List Line -> List Line -> List Line
ifThenElse cond t f =
    List.concat
        [ [ Table [[ keywordToken "if", parenToken "("] ++ cond ++[parenToken ")", parenToken "{"]] , Indent ]
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
