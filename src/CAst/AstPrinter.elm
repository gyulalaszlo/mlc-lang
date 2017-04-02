module CAst.AstPrinter exposing
    ( statementListToString, functionToString, Token

    )
{-| Describe me please...
|-}

import CAst exposing (BinaryOp(..), Expression, ExpressionBody(..), FunctionStatement, Statement(..), StatementList)
import CAsm.SymbolType exposing (typeToCCode)

type alias Token =
    { class: String
    , tag: String
    , text: String
    }

functionToString :  FunctionStatement -> List Token
functionToString f =
    let
        arg (n,t) =
            [ typeToken "function.arg.type" (typeToCCode t)
            , symbolToken "function.arg.name" n
            ]

        fnHead =
            List.concat <|
                [   [ typeToken "function.returns" (typeToCCode f.returns)
                    , functionNameToken "function.name" f.name
                    , parenToken "function.paren.open" "("
                    ]
                , List.map arg f.args
                        |> List.intersperse [(colonToken "function.arg.colon" ",")]
                        |> List.concat
                , [ parenToken "function.paren.close" ")" ]
                ]
    in
        fnHead ++ (bracedStatementList "function.braces" f.body)




statementListToString :  StatementList -> List Token
statementListToString s =
    List.concatMap statementToString s


prefixedParen : String -> Expression -> List Token
prefixedParen prefix e =
    List.concat
        [ [ keywordToken (prefix ++ ".keyword") prefix
        , parenToken (prefix ++ ".paren.open") "(" ]
        , expression e
        , [parenToken (prefix ++ ".paren.close") ")"]
        ]


bracedStatementList : String -> StatementList -> List Token
bracedStatementList baseScope ss =
    List.concat
        [ [ braceToken (baseScope ++ ".open") "{" ]
        , statementListToString ss
        , [braceToken (baseScope ++ ".close") "}" ]
        ]


statementToString : Statement -> List Token
statementToString s =
    case s of

        SComment c ->
            List.concat
                [ [ commentToken "comment.block.open" "/*"]
                , List.map (commentToken "comment.block.line") c
                , [ commentToken "comment.block.close" "*/"]
                ]
--            [ commentToken "comment.block" <| "/*\n" ++ String.join "\n" c  ++ "\n*/" ]

        SLValueDeclare {name, type_} ->
            [  typeToken "declare.type" (typeToCCode type_)
            ,  symbolToken "declare.name" name
            ,  semiToken "declare.semi" ";"
            ]

        SLValueAssign {name, type_, value} ->
            List.concat
                [ [ symbolToken "assign.name" name ]
                , [assignToken "assign.op" "="]
                , expression value
                , [ semiToken "assign.semi" ";"]
                ]

        SWhile {condition, body} ->
            (prefixedParen "while" condition) ++ bracedStatementList "while.braces" body

        SIf {condition, true, false} ->
            List.concat
                [ prefixedParen "if" condition
                , bracedStatementList "if.then.braces" true
                , [keywordToken "if.else.keyword" "else"]
                , bracedStatementList "if.else.braces" false
                ]

        SReturn e ->
            List.concat
                [ [ keywordToken "return.keyword" "return" ]
                , expression e
                , [ semiToken "return.semi" ";" ]
                ]

        SContinue ->
            [ keywordToken "continue.keyword" "continue"
            , semiToken "continue.semi" ";"
            ]

        SBreak -> [keywordToken "break.keyword" "break", semiToken "break.semi" ";" ]
        SGoTo l -> [keywordToken "goto.keyword" "goto", labelToken "goto.label" l, semiToken "goto.semi" ";"]
        SLabel l -> [labelToken "label.name" (l ++ ":")]

        SFunctionDefinition f -> []
        SFunctionDeclaration f -> functionToString f


expression : Expression -> List Token
expression e =
    expressionBody e.body

expressionBody : ExpressionBody -> List Token
expressionBody e =
    case e of
        ExprWithComment c body ->
            (commentToken "comment.line" <| "//" ++ c) :: expressionBody body

        ExprFunctionCall n args ->
            List.concat
                [ [functionNameToken "expr.call.name" n, parenToken "expr.call.paren.open" "("]
                , List.concatMap expression args
                , [parenToken "expr.call.paren.close" ")"]
                ]

        ExprLValue n -> [symbolToken "expr.lvalue" n]

        ExprBinary operator left right ->
            [parenToken "expr.binary.paren.open" "("]
            ++ binaryOpToString operator left right
            ++ [ parenToken "expr.binary.paren.close" ")" ]

        ExprLiteral { text } -> [ literalToken "literal" text]




binaryOpToString : BinaryOp -> Expression -> Expression -> List Token
binaryOpToString  operator left right =
    let mid s = expression left ++ [opToken "expr.binary.op" s] ++ expression right
    in case operator of
        BinaryPlus ->  mid "+"
        BinaryMinus ->  mid "-"
        BinaryTimes ->  mid "*"
        BinaryDividedBy ->  mid "/"


        BinaryNth ->
            List.concat
                [ expression right
                , [opToken "expr.binary.nth.bracket.open" "["]
                , expression left
                , [opToken "expr.binary.nth.bracket.close" "]"]
                ]
        CompLt -> mid "<"
        CompGt -> mid ">"
        CompEq -> mid "=="
        CompNeq -> mid "!="





{-
    TOKEN TYPES
    ===========
-}

symbolToken = token "symbol"
labelToken = token "label"
typeToken = token "type"
parenToken = token "parenthesis"
braceToken = token "brace"
functionNameToken = token "functionName"
semiToken = token "semicolon"
colonToken = token "colon"
assignToken = token "assign"
opToken = token "op"
commentToken = token "comment"
keywordToken = token "keyword"
literalToken = token "literal"



token : String -> String -> String -> Token
token c = Token c

