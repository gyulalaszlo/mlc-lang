module CAsm.AstPrinter exposing
    ( functionToString

    , defaultCodeStyle, applyCodeStyle
    )
{-| Describe me please...
|-}

import CAsm.CAst exposing (..)
import CAsm.SymbolType exposing (typeToCCode)
import Codegen.Indented exposing (Line(..), Token, applyIndents)
import Dict exposing (Dict)

type SideWs
    = NoSpace
    | Space
    | LineBreak

type WsIndent
    = Indent
    | Outdent
    | NoIndent

type alias Ws =
    { left: SideWs
    , right: SideWs
    , indentLeft: WsIndent
    , indentRight: WsIndent
    }

emptyWs : Ws
emptyWs = { left = NoSpace, right = NoSpace, indentLeft = NoIndent, indentRight = NoIndent }

noSpace : Ws
noSpace = emptyWs

spaceLeft : Ws
spaceLeft = { emptyWs | left = Space }

spaceRight : Ws
spaceRight = { emptyWs | right = Space }

spaceBoth : Ws
spaceBoth = { emptyWs | left = Space, right = Space }

breakBefore : Ws
breakBefore = {  emptyWs |left = LineBreak}

breakAfter : Ws
breakAfter = { emptyWs |  right = LineBreak }

breakBoth : Ws
breakBoth = { emptyWs | left = LineBreak, right = LineBreak }

indentBefore : Ws
indentBefore = { emptyWs | left = LineBreak, indentLeft = Indent}

indentAfter : Ws
indentAfter = { emptyWs |  indentRight = Indent }

outdentBefore : Ws
outdentBefore = { emptyWs | indentLeft = Outdent, left = LineBreak}

outdentAfter : Ws
outdentAfter = { emptyWs |  indentRight = Outdent }

blockOpen = { emptyWs | left = Space, right = LineBreak, indentRight = Indent }
blockClose = { emptyWs | indentLeft = Outdent, left = LineBreak, right = LineBreak }

type alias CodeStyle = Dict String Ws

defaultCodeStyle : CodeStyle
defaultCodeStyle =
    Dict.fromList
        [ ("function.name", spaceLeft)
        , ("function.returns", noSpace)
        , ("function.paren.open", spaceRight)
        , ("function.paren.close", spaceBoth)
        , ("function.arg.type", noSpace)
        , ("function.arg.name", spaceLeft)
        , ("function.arg.colon", spaceRight)
        , ("function.braces.open", blockOpen)
        , ("function.braces.close", blockClose)

        , ("while.paren.open", noSpace)
        , ("while.paren.close", noSpace)
        , ("while.braces.open", blockOpen)
        , ("while.braces.close", blockClose)

        , ("comment.block", breakAfter)

        , ("declare.type", noSpace)
        , ("declare.name", spaceLeft)
        , ("declare.semi", breakAfter)

        , ("assign.name", spaceRight)
        , ("assign.expr", spaceLeft)
        , ("assign.semi", breakAfter)

        , ("expr.call.name", noSpace)
        , ("expr.call.paren.open", noSpace)
        , ("expr.call.paren.close", noSpace)

        , ("expr.binary.op", spaceBoth)
        , ("expr.binary.paren.open", noSpace)
        , ("expr.binary.paren.close", noSpace)
        , ("expr.binary.nth.bracket.open", noSpace)
        , ("expr.binary.nth.bracket.close", noSpace)

        , ("expr.lvalue", noSpace)

        , ("if.paren.open", noSpace)
        , ("if.paren.close", noSpace)
        , ("if.then.braces.open", blockOpen)
        , ("if.then.braces.close", outdentBefore)
        , ("if.else.braces.open", blockOpen)
        , ("if.else.braces.close", blockClose)
        , ("if.else.keyword", spaceBoth)
        , ("if.keyword", spaceRight)

        , ("literal", noSpace)

        , ("continue.keyword", noSpace)
        , ("continue.semi", breakAfter)

        , ("return.keyword", spaceRight)
        , ("return.semi", breakAfter)
        ]



wss : String -> List Token -> List Token
wss w ts = List.map (\t -> { t | tag = w }) ts

applyWs : Int -> Int -> Ws -> Token -> List Token
applyWs indentL indentR {left, right} t =
    List.concat [sideWs indentL t.tag left, [t], sideWs indentR t.tag right]

sideWs : Int -> String -> SideWs -> List Token
sideWs indent tag w =
    let indentStr = String.repeat indent "\t"
    in case w of
        NoSpace -> []
        Space -> [whiteSpaceToken tag " "]
        LineBreak -> [{ lineBreakToken | tag = tag }, whiteSpaceToken tag <| indentStr ]


applyCodeStyle : CodeStyle -> List Token -> List Token
applyCodeStyle cs ts =
    List.foldl (applyCodeStyleHelepr cs) (0,[]) ts
        |> Tuple.second

applyCodeStyleHelepr : CodeStyle -> Token -> (Int, List Token) -> (Int, List Token)
applyCodeStyleHelepr cs t (indent, ts) =
    let
        tokenWs = Dict.get t.tag cs |> Maybe.withDefault spaceRight
        (indentL, indentR) =
            case (tokenWs.indentLeft, tokenWs.indentRight) of
                (Indent, Indent) -> (indent + 1, indent + 2)
                (Indent, Outdent) -> (indent + 1, indent)
                (Outdent, Indent) -> (max 0 <| indent - 1, indent)
                (Outdent, Outdent) -> (max 0 <| indent - 1, max 0 <| indent - 2)
                (Indent, NoIndent) -> (indent + 1, indent + 1)
                (Outdent, NoIndent) -> (max 0 <| indent - 1, max 0 <| indent - 1)
                (NoIndent, Indent) -> (indent, indent + 1)
                (NoIndent, Outdent) -> (indent, max 0 <| indent - 1)
                (NoIndent, NoIndent) -> (indent, indent)

    in
        ( indentR
        , ts ++ applyWs indentL indentR tokenWs t
        )


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
            [ commentToken "comment.block" <| "/*\n" ++ String.join "\n" c  ++ "\n*/" ]

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
whiteSpaceToken = token "whiteSpace"
lineBreakToken = token "line-break" "" "\n"
indentToken = token "indent" "" ""
outdentToken = token "outdent" "" ""



token : String -> String -> String -> Token
token c = Token c

tokens : List Token -> Line
tokens ts =
    Table [ts]
