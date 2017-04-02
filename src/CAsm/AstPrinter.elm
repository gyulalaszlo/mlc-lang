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

type alias Ws =
    { left: SideWs
    , right: SideWs
    }

noSpace = { left = NoSpace, right = NoSpace }
spaceLeft = { left = Space, right = NoSpace }
spaceRight = { left = NoSpace, right = Space }
spaceBoth = { left = Space, right = Space }

breakBefore = { left = LineBreak, right = NoSpace }
breakAfter = { left = NoSpace, right = LineBreak }
breakBoth = { left = LineBreak, right = LineBreak }

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
        , ("function.braces.open", breakAfter)
        , ("function.braces.close", breakAfter)

        , ("while.paren.open", noSpace)
        , ("while.paren.close", noSpace)
        , ("while.braces.open", breakAfter)
        , ("while.braces.close", breakAfter)

        , ("comment.block", breakBoth)

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
        , ("if.then.braces.open", breakAfter)
        , ("if.then.braces.close", noSpace)
        , ("if.else.braces.open", breakAfter)
        , ("if.else.braces.close", breakAfter)
        , ("if.else.keyword", spaceBoth)
        , ("if.keyword", spaceRight)

        , ("literal", noSpace)

        , ("continue.keyword", noSpace)
        , ("continue.semi", breakAfter)

        , ("return.keyword", spaceRight)
        , ("return.semi", breakAfter)
        ]

--type alias CodeLayout =
--    { function:
--        { name: Ws
--        , returns: Ws
--        , openParen: Ws
--        , closeParen: Ws
--        , argColon: Ws
--        , argType: Ws
--        , argName: Ws
--        }
--    , comment: Ws
--    , declare: { type_: Ws, name: Ws, semi: Ws }
--    , assign: { name: Ws, expr: Ws, semi: Ws }
--    , return: { keyword: Ws, expr: Ws, semi: Ws }
--    , continue: { keyword: Ws, semi: Ws }
--    }

ws : String -> Token -> List Token
ws w t = wss w [t]

wss : String -> List Token -> List Token
wss w ts = List.map (\t -> { t | tag = w }) ts

applyWs : Ws -> Token -> List Token
applyWs {left, right} t =
    List.concat [sideWs left, [t], sideWs right]

sideWs : SideWs -> List Token
sideWs w =
    case w of
        NoSpace -> []
        Space -> [whiteSpaceToken "" " "]
        LineBreak -> [whiteSpaceToken "" "\n"]


applyCodeStyle : CodeStyle -> List Token -> List Token
applyCodeStyle cs ts =
    let tokenWs {tag} = Dict.get tag cs |> Maybe.withDefault spaceRight
    in List.concatMap (\t -> applyWs (tokenWs t) <| t) ts

--defaultCodeLayout : CodeLayout
--defaultCodeLayout =
--    { function =    { name = spaceLeft
--                    , returns = noSpace
--                    , openParen = spaceRight
--                    , closeParen = spaceBoth
--                    , argColon = spaceRight
--                    , argType = noSpace
--                    , argName = spaceLeft
--                    , braces = { open = breakAfter, close = breakAfter}
--                    }
--    , comment = breakBoth
--    , declare = { type_ = noSpace, name = spaceLeft, semi = breakAfter }
--    , assign = { name = spaceRight, expr = spaceLeft, semi = breakAfter }
--    , return = { keyword = noSpace, expr = spaceLeft, semi = breakAfter }
--    , continue = { keyword = noSpace, semi = breakAfter }
--    }


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



--astToString : StatementList -> List Token
--astToString = statementListToString


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
                , wss "return.expr" <| expression e
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

--        _ -> []



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
--        _ ->  []





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



token : String -> String -> String -> Token
token c = Token c

tokens : List Token -> Line
tokens ts =
    Table [ts]
