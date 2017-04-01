module CAsm.AstPrinter exposing (astToString)
{-| Describe me please...
|-}

import CAsm.CAst exposing (..)
import CAsm.SymbolType exposing (typeToCCode)
import Codegen.Indented exposing (Line(..), Token, applyIndents)

astToString : StatementList -> String
astToString s =
    List.concatMap statementToString s
        |> applyIndents
        |> String.join "\n"

statementListToString : StatementList -> List Line
statementListToString s =
    List.concatMap statementToString s

prefixedParen : String -> Expression -> List Token
prefixedParen prefix e =
    List.concat
        [ [ keywordToken prefix, parenToken "(" ]
        , expression e
        , [parenToken ")"]
        ]
bracedStatementList : StatementList -> List Line
bracedStatementList ss =
    List.concat
        [ [ tokens [braceToken "{"], Indent ]
        , statementListToString ss
        , [ Outdent, tokens [braceToken "}"]]
        ]


statementToString : Statement -> List Line
statementToString s =
    case s of

        SComment c ->
            List.concat
                [ [tokens [commentToken "/*"], Indent ]
                , List.map (\l -> tokens [commentToken l] ) c
                , [ Outdent, tokens [commentToken "*/"]]
                ]
        SLValueDeclare {name, type_} ->
            [ tokens <| [ typeToken (typeToCCode type_), symbolToken name, semiToken]]

        SLValueAssign {name, type_, value} ->
            [ tokens <| [ symbolToken name, assignToken] ++ expression value ++ [semiToken]]

        SWhile {condition, body} ->
            (tokens <| prefixedParen "while" condition) :: bracedStatementList body

        SIf {condition, true, false} ->
            List.concat
                [ (tokens <| prefixedParen "if" condition) :: bracedStatementList true
                , (tokens [keywordToken "else"] ) :: bracedStatementList false
                ]

        SReturn e -> [ (tokens <| (keywordToken "return") :: expression e ++ [semiToken]) ]
        SContinue -> [ (tokens [keywordToken "continue", semiToken]) ]
        SBreak -> [ (tokens [keywordToken "break", semiToken]) ]
        SGoTo l -> [ (tokens [keywordToken "goto", labelToken l, semiToken]) ]
        SLabel l -> [ (tokens [keywordToken "label", labelToken (l ++ ":"), semiToken]) ]



expression : Expression -> List Token
expression e =
    expressionBody e.body

expressionBody : ExpressionBody -> List Token
expressionBody e =
    case e of
        ExprWithComment c body ->
            (commentToken <| "//" ++ c) :: expressionBody body

        ExprFunctionCall n args ->
            List.concat
                [ [functionNameToken n, parenToken "("]
                , List.concatMap expression args
                , [parenToken ")"]
                ]

        ExprLValue n -> [symbolToken n]

        ExprBinary operator left right ->
            [parenToken "("]
            ++ binaryOpToString operator left right
            ++ [ parenToken ")" ]

        ExprLiteral { text } -> [ literalToken text]

--        _ -> []



binaryOpToString : BinaryOp -> Expression -> Expression -> List Token
binaryOpToString  operator left right =
    let mid s = expression left ++ [opToken s] ++ expression right
    in case operator of
        BinaryPlus ->  mid "+"
        BinaryMinus ->  mid "-"
        BinaryTimes ->  mid "*"
        BinaryDividedBy ->  mid "/"


        BinaryNth -> expression right ++ [opToken "["] ++ expression left ++ [opToken "]"]
        CompLt -> mid "<"
        CompGt -> mid ">"
        CompEq -> mid "=="
        CompNeq -> mid "!="
--        _ ->  []





{-
    TOKEN TYPES
    ===========
-}

symbolToken = Token "symbol"
labelToken = Token "label"
typeToken = Token "type"
parenToken = Token "parenthesis"
braceToken = Token "brace"
functionNameToken = Token "functionName"
semiToken = Token "semicolon" ";"
colonToken = Token "colon" ","
assignToken = Token "assign" "="
opToken = Token "op"
commentToken = Token "comment"
keywordToken = Token "keyword"
literalToken = Token "literal"


tokens : List Token -> Line
tokens ts =
    Table [ts]
