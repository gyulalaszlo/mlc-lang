module Codegen.Codegen exposing (toCCode)

import Codegen.Lang exposing (..)

-- ======================================


identifierToString : Identifier -> String
identifierToString i =
    i

typeToString : TypeIdentifier -> String
typeToString i =
    case i of
        Void -> "void"
        Integral s -> s
        Structure s -> "struct " ++ s
        StaticArray size t ->
            typeToString t ++ "[" ++ toString size ++ "]"
        Pointer t ->
            typeToString t ++ "*"
        Const t ->
            "const " ++ typeToString t



-- ======================================

arithmeticOperatorToString : BinaryOperator -> String
arithmeticOperatorToString op =
    case op of
        OpIntPlus -> "+"
        OpIntMinus -> "-"
        OpIntMultiply -> "*"
        OpIntDivision -> "/"
        OpIntModulo -> "%"
        OpIntRShift -> ">>"
        OpIntLShift -> "<<"


        OpCompEq -> "=="
        OpCompNeq -> "!="
        OpCompGt -> ">"
        OpCompGte -> ">="
        OpCompLt -> "<"
        OpCompLte -> "<="

        OpBoolAnd -> "&&"
        OpBoolOr -> "||"
        -- TODO: XOR = ^^ ? :)
        OpBoolXor -> "^^"


        OpAssign -> "="



expressionToCode : Expression -> String
expressionToCode e =
    case e of
        IntegerLiteral valueStr ->
            valueStr

        ValueOf ident ->
            (identifierToString ident)

        BinaryExpression op a b ->
            "("
                ++ (expressionToCode a)
                ++ " "
                ++ (arithmeticOperatorToString op)
                ++ " "
                ++ (expressionToCode b)
                ++ ")"

type Line
    = Text String
    | Indent
    | Outdent


--type Indent
--    = BeforeLine Int
--    | AfterLine Int
--    | NoIndent
--
--
--type alias Line =
--    { contents : String
--    , indent : Indent
--    }


mapJoin : String -> (a -> String) -> List a -> String
mapJoin joiner f lst =
    List.map f lst |> String.join joiner




concatLine : List String -> List Line
concatLine bits =
    [ Text ((String.join " " bits) ++ ";") ]

bracedBody : Statements -> List Line
bracedBody s =
    List.concat
        [ [ Text "{", Indent ]
        , List.concatMap statementToCode s
        , [ Outdent, Line "}" ]
        ]


statementToCode : Statement -> List Line
statementToCode s =
    case s of
        DeclareLocal t localName ->
            concatLine
                [ typeToString t
                , identifierToString localName
                ]

        ExpressionStatement expr ->
            concatLine
                [ expressionToCode expr ]

        Return expr ->
            concatLine
                [ "return"
                , expressionToCode expr
                ]

        ForLoop init const step body ->
            List.concat
                [ concatLine
                    [ "for ("
                    , String.join "; "
                        <| List.map expressionToCode [init, const, step]
                    , ")"
                    ]
                , bracedBody body
                ]

        IfStatement condition body ->
            List.concat
                [ [ Text ("if " ++ (expressionToCode condition) ++ " {")  ]

                , bracedBody body
--                , List.concatMap statementToCode body
--                , [ Line "}" (BeforeLine -1) ]
                ]

        WhileLoop cond body ->
            List.concat
                [ concatLine
                    [ "while ("
                    , expressionToCode cond
                    , ")"
                    ]
                , bracedBody body
                ]

        LabelDeclaration name ->
            [ Text (name ++ ":") ]
--            concatLine
--                [ name ++ ":"
--                ]

type alias AppendLineIndentState = { indent : Int, lines : List String }

applyLineIndent : Line -> AppendLineIndentState -> AppendLineIndentState
applyLineIndent line state =
    let
        withNewLine =
            state.lines ++ [(String.repeat state.indent "\t")]

    in
        case line of
            Text s ->
                { state | lines = withNewLine s }

            Indent ->
                { state | indent = state.indent + 1 }

            Outdent ->
                { state | indent = state.indent + 1 }




toCCode : List Statement -> List String
toCCode s =
    let
        init =
            { lines = [], indent = 0 }

        indentedLines =
            List.foldl applyLineIndent
                init
                (List.concatMap statementToCode s)
    in
        indentedLines.lines
