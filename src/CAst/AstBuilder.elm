module CAst.AstBuilder exposing (toAst, toFunction)
{-| Describe me please...
|-}


import CAsm exposing (..)
import CAsm.Error as Error exposing (Error, wrapError)
import CAsm.FlowGraph exposing (FlowPath(..), flowGraphFor, hasJumpTo, isBlockALoop, isJumpALoop, loopEdges, loopNodes)
import CAsm.SymbolType as SymbolType exposing (BitWidth(..), SymbolType(..), bool, typeToString, unwrapPointer)
import CAst exposing (..)
import Dict
import List.Extra


toFunction : CAsm -> StatementList -> FunctionStatement
toFunction c sl =
    { name = c.name
    , args = List.map (\{name, type_} -> (name, type_)) c.parameters
    , returns = c.returnType
    , body = sl
    }

{-| Converts a CAsm assembly to C Code
|-}
toAst : CAsm -> Result Error StatementList
toAst c =
    let

        isHead : (a -> Bool) -> List a -> Bool
        isHead pred xs =
            case xs of
                [] -> False
                x :: xs -> pred x

        visibleBlocks bs =
          List.filter (\b -> isHead (\bb ->  bb.name == b.name) bs || (not <| canHoistBlock c b)) bs

        header =
            (SComment <| flowPathsAsComment c) :: symbolsUsed c
    in
        visibleBlocks c.blocks
            |> List.map (blockToCode c)
            |> Error.wrapErrors ("Error while compiling blocks for Assembly:" ++ toString c.name)
            |> Result.map (\bs -> header ++ (List.concat bs))


{-| Converts a single block to C code with labels, indentation and instructions
|-}
blockToCode : CAsm -> Blk -> Result Error StatementList
blockToCode c b =
    blockInner c "" b
        |> Error.wrapErrorMsg ["While compiling block inner: ", b.name]
        |> Result.andThen (blockWrapper c b)


{-| Outputs the correct header for a block (label, loop or nothing) and wraps the contents
potentially in braces
|-}
blockWrapper : CAsm -> Blk -> StatementList -> Result Error StatementList
blockWrapper c b lines =
    if isBlockALoop c b then
        Ok lines
    else
        if hasJumpTo c b.name then
            Ok <| [ SLabel b.name ] ++ lines
        else
            Ok <| lines


loopHead : CAsm -> LabelName -> Blk -> Result Error StatementList
loopHead c from b =
    let
        inline = inlineArgs c from

    in
        case b.exit of
            Branch {condition, true, false} ->
                Result.map3 (\cond t f -> (makeWhile cond t) :: f  )
                    (inline condition)
                    (findBlock true c |> Result.andThen (blockInner c b.name) )
                    (findBlock false c |> Result.andThen (blockInner c b.name) )

            _ -> Ok  [ makeWhile ( makeLiteral bool "true") [] ]


blockInnerWrapper : CAsm -> LabelName -> Blk -> StatementList -> Result Error StatementList
blockInnerWrapper c l b inner =
    let
        isLoop = isBlockALoop c b

    in
        if isLoop
            then loopHead c l b |> Result.map (\h -> List.append h inner)
            else
                exit c l b
                    |> Result.map (\s -> inner ++ s )
                    |> Error.wrapErrorMsg
                        ["While wrapping block inner", toString b.name, "coming from", toString l]

{-| Shared functionality for the actual instructions in the block sans the label and the indentation.
|-}
blockInner : CAsm -> LabelName -> Blk -> Result Error StatementList
blockInner c from b =
            lets c from  (List.filter (\l -> valueKind c l.name == LValue) b.lets)
                |> Error.wrapErrorMsg
                    ["While generating lets for block inner", toString b.name
                    ,"coming from", toString from]
                |> Result.andThen (blockInnerWrapper c from b)


{-| Generates the exit part of the block
|-}
exit: CAsm -> LabelName -> Blk -> Result Error StatementList
exit c from b =
    case b.exit of
        Return s ->
            (inlineArgs c from s)
                |> Result.map (\expr -> [SReturn expr])
                |> Error.wrapError "While looking up Return exit"
        Branch e ->
            branchExit c b e
                |> Result.map List.singleton
                |> Error.wrapError "While looking up Branch exit"
        JumpNext ->
            nextNodeOf c.blocks b
                |> Result.fromMaybe
                    (Error.make <| "Cannot find next node for JumpNext node" ++ b.name)
                |> Result.andThen (\next -> hoistPhiLets c b.name next.name)

{-| Specialization of exit for branch exits
|-}
branchExit : CAsm -> Blk -> BranchExit -> Result Error Statement
branchExit c b e =
    let
        goto = hoisted c b.name
    in
        Result.map3
            (\ cond t f -> makeIf cond t f)
            (inlineArgs c b.name e.condition)
            (goto e.true)
            (goto e.false)


hoisted : CAsm -> LabelName -> LabelName -> Result Error StatementList
hoisted c from to =
    let
        hoists to = hoistPhiLets c from to
        goto to =
             Result.map2
                    (\hoists hb ->
                        case hb of
                            Nothing -> hoists ++ (gotoOrContinue c from to)
                            Just statements -> hoists ++ statements)
                    (hoists to)
                    (hoistBlocks c from to)
    in
        goto to



gotoOrContinue : CAsm -> LabelName -> LabelName -> StatementList
gotoOrContinue c from to =
    if isJumpALoop c from to then [ SContinue ] else [ SGoTo to ]



{-| Emits code to update all phi-lets that the result of jumping from `from` to `to` would update.
|-}
hoistPhiLets : CAsm -> LabelName -> LabelName -> Result Error StatementList
hoistPhiLets c from to =
    let
        findInPhis b =
            findBy Tuple.first from b.phis
                |> Result.fromMaybe (Error.make "Cannot find Phi let")

        generatePhiLets m =
            case m of
                Nothing -> Ok []
                Just (_, phis) ->
                    lets c from (List.filter (isLValue c << .name) phis)

    in
        findBy .name to c.blocks
            |> Maybe.andThen (\b -> findBy Tuple.first from b.phis)
            |> generatePhiLets






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
hoistBlocks : CAsm -> LabelName -> LabelName -> Result Error (Maybe StatementList)
hoistBlocks c from to =
    findBlock to c
        |> Error.wrapErrorMsg ["While trying to hoist block for jump from", from, "to", to]
        |> Result.andThen (\b ->
            if canHoistBlock c b
                then
                    blockInner c from b
                        |> Result.map Just
                        |> Error.wrapErrorMsg ["While generating block inner", b.name]
                else
                    Ok Nothing )

{-| Emits C code for declaring all symbols that will be used
|-}
symbolsUsed : CAsm -> StatementList
symbolsUsed c =
    let
        refCount s = "Refs: " ++ toString (symbolUseCount c s.name)
        rval s = toString (valueKind c s.name)
    in
        c.symbols
            |> List.filter (\{valueKind} -> valueKind == LValue)
            |> List.map (\s -> makeLValuleDeclare s.name s.type_ )

{-| Wraps outputting a function call as C code.

Handles builtins and C code.

|-}
functionCall : CAsm -> LabelName -> FunctionName -> FunctionArgs -> Result Error Expression
functionCall c label f a =
    let
        inline = inlineArgs c label

        lookupFunction f =
            findBy .name f c.calls
                |> Result.fromMaybe (Error.make <| "Cannot find function" ++ f.name)

        callArgs n =
            List.map inline a
                |> Error.wrapErrors ("While resolving call args for: " ++ n)
                |> Result.map (ExprFunctionCall n)

        call n =
            Result.map2
                (\inner {returnType} -> makeExpression returnType inner)
                (callArgs n)
                (lookupFunction f)

        binary t o l r =
            Result.map2 (makeBinary t o) (inline l) (inline r)

        constant n =
            findConstant n c
                |> Result.map (\(_,val) -> makeLiteral IntegerLiteral val)

        binaryMath = binaryMathOp c label
    in
        case f.package of
            Pkg pkg -> call <| pkg ++ "_" ++ f.name
            ExternC -> call <| f.name
            Builtin pkg ->
                case (f.name, a) of
                    ("eq", [l,r]) -> binary bool CompEq l r
                    ("lt", [l,r]) -> binary bool CompLt l r
                    ("gt", [l,r]) -> binary bool CompGt l r
                    ("at", [l,r]) ->
                        let ll = inline l
                            rr = inline r
                        in
                            ll
                                |> Result.andThen
                                    (\{meta} -> unwrapPointer meta.returns)
                                |> Result.map3
                                    (\ll rr tt -> makeBinary tt BinaryNth rr ll)
                                    (inline l)
                                    (inline r)
                                |> Error.wrapErrorMsg ["While trying to call Nth"]

                    ("plus", [l,r]) -> binaryMath BinaryPlus l r
                    ("alias", [r]) -> inlineArgs c label r
                    ("from-const", [r]) -> constant r
                    _ -> call <| pkg ++ "_" ++ f.name


{-| Builds a binary math expression and does basic type checking using SymbolType.concat
-}
binaryMathOp : CAsm -> LabelName -> BinaryOp -> SymbolName -> SymbolName -> Result Error Expression
binaryMathOp c from o l r =
    let
        inline = inlineArgs c from
        ll = inline l
        rr = inline r
        typesMatch ll rr =
            SymbolType.concat ll.meta.returns rr.meta.returns
        typeMatchErr ll rr =
            Error.make <| String.join " " <|
                [ "Types for", toString o, "do not match."
                , "| left:", typeToString ll.meta.returns
                , "| right:", typeToString rr.meta.returns
                ]
    in
        Result.map2 (\ll rr -> (ll,rr)) ll rr
            |> Result.andThen
                (\(ll,rr)->
                    typesMatch ll rr
                        |> Result.fromMaybe (typeMatchErr ll rr)
                        |> Result.map (\tt -> (tt,ll,rr))
                    )
            |> Result.map (\(tt,ll,rr)-> makeBinary tt o ll rr)


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



{-| Returns the flow paths through a function as comments in the C code
|-}
flowPathsAsComment : CAsm -> List String
flowPathsAsComment c =
    let
        (flow, _) = flowGraphFor c
        path p = String.join " -> " p
        flowLine f =
            case f of
                EndsWithReturn p ->
                     "RETURN: " ++ (path p)
                EndsWithLoop b n a ->
                     "LOOP: " ++ (path b) ++ " -> (" ++ n ++ " -> " ++ (path a) ++ " -> ...)"
    in
        List.map flowLine flow


{-| Outputs a list of let expressions.
|-}
lets : CAsm -> LabelName -> List Let -> Result Error StatementList
lets c ln ls =
    let
        aLet l =
            case valueKind c l.name of
                LValue -> letLValue c ln l
                RValue -> letRValue c ln l
    in
        List.map aLet ls
            |> Error.wrapErrors ("While looking up lets for label " ++ toString ln ++ "")


letLValue : CAsm -> LabelName -> Let -> Result Error Statement
letLValue c ln l =
    let callResult = functionCall c ln l.fn l.args
    in findSymbol l.name c
        |> Result.map2
            (\res {name, type_} -> makeLValueAssign name type_ res)
            callResult

letRValue : CAsm -> LabelName -> Let -> Result Error Statement
letRValue = letLValue


{-| Tries to inline the arguments for a function call recursively until it hits an lvalue
|-}
inlineArgs : CAsm -> LabelName -> SymbolName -> Result Error Expression
inlineArgs c l n =
    case valueKind c n of
        LValue ->
            findSymbol n c
                |> Result.map (\{name, type_} -> makeLValueExpr name type_)
                |> Error.wrapErrorMsg ["While trying to find LValue", toString n]
        RValue ->
            findSymbolDefinition n l c
                |> Result.andThen (\{fn, args} -> functionCall c l fn args)
                |> Error.wrapErrorMsg ["While trying to find RValue", toString n]



