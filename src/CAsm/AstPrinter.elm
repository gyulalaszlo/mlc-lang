module CAsm.AstPrinter exposing (functionToString, defaultCodeLayout)
{-| Describe me please...
|-}

import CAsm.CAst exposing (..)
import CAsm.SymbolType exposing (typeToCCode)
import Codegen.Indented exposing (Line(..), Token, applyIndents)

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

type alias Braces =
    { open: Ws, close: Ws }

type alias CodeLayout =
    { function:
        { name: Ws
        , returns: Ws
        , openParen: Ws
        , closeParen: Ws
        , argColon: Ws
        , argType: Ws
        , argName: Ws
        , braces: Braces
        }
    , comment: Ws
    , declare: { type_: Ws, name: Ws, semi: Ws }
    , assign: { name: Ws, expr: Ws, semi: Ws }
    , return: { keyword: Ws, expr: Ws, semi: Ws }
    , continue: { keyword: Ws, semi: Ws }
    }

ws : Ws -> Token -> List Token
ws w t = wss w [t]

wss : Ws -> List Token -> List Token
wss w ts = List.concat [ (sideWs w.left), ts, (sideWs w.right)]

sideWs : SideWs -> List Token
sideWs w =
    case w of
        NoSpace -> []
        Space -> [whiteSpaceToken " "]
        LineBreak -> [whiteSpaceToken "\n"]


defaultCodeLayout : CodeLayout
defaultCodeLayout =
    { function =    { name = spaceLeft
                    , returns = noSpace
                    , openParen = spaceRight
                    , closeParen = spaceBoth
                    , argColon = spaceRight
                    , argType = noSpace
                    , argName = spaceLeft
                    , braces = { open = breakAfter, close = breakAfter}
                    }
    , comment = breakBoth
    , declare = { type_ = noSpace, name = spaceLeft, semi = breakAfter }
    , assign = { name = spaceRight, expr = spaceLeft, semi = breakAfter }
    , return = { keyword = noSpace, expr = spaceLeft, semi = breakAfter }
    , continue = { keyword = noSpace, semi = breakAfter }
    }


functionToString : CodeLayout -> FunctionStatement -> List Token
functionToString cl f =
    let
        arg (n,t) =
            List.concat
                [ ws cl.function.argType <| typeToken (typeToCCode t)
                , ws cl.function.argName <| symbolToken n
                ]

        fnHead =
            List.concat <|
                [ ws cl.function.returns <| typeToken (typeToCCode f.returns)
                , ws cl.function.name <| functionNameToken f.name
                , ws cl.function.openParen <| parenToken "("
                , List.map arg f.args
                        |> List.intersperse (ws cl.function.argColon colonToken)
                        |> List.concat
                , ws cl.function.closeParen <| parenToken ")"
                ]
    in
        fnHead ++ (bracedStatementList cl cl.function.braces f.body)



--astToString : StatementList -> List Token
--astToString = statementListToString


statementListToString : CodeLayout -> StatementList -> List Token
statementListToString cl s =
    List.concatMap (statementToString cl) s


prefixedParen : String -> Expression -> List Token
prefixedParen prefix e =
    List.concat
        [ [ keywordToken prefix, parenToken "(" ]
        , expression e
        , [parenToken ")"]
        ]


bracedStatementList : CodeLayout -> Braces -> StatementList -> List Token
bracedStatementList cl  {open, close} ss =
    List.concat
        [ ws open <| braceToken "{"
        , statementListToString cl ss
        , ws close <| braceToken "}"
        ]


statementToString : CodeLayout -> Statement -> List Token
statementToString cl s =
    case s of

        SComment c ->
            ws cl.comment <| commentToken <| "/*\n" ++ String.join "\n" c  ++ "\n*/"

        SLValueDeclare {name, type_} ->
            List.concat
                [ ws cl.declare.type_ <| typeToken (typeToCCode type_)
                , ws cl.declare.name <|  symbolToken name
                , ws cl.declare.semi <| semiToken
                ]

        SLValueAssign {name, type_, value} ->
            List.concat
                [ ws cl.assign.name <|  symbolToken name
                , [assignToken]
                , wss cl.assign.expr <| expression value
                , ws cl.assign.semi <| semiToken
                ]

        SWhile {condition, body} ->
            (prefixedParen "while" condition) ++ bracedStatementList cl cl.function.braces body

        SIf {condition, true, false} ->
            List.concat
                [ prefixedParen "if" condition
                , bracedStatementList cl cl.function.braces true
                , [keywordToken "else"]
                , bracedStatementList cl cl.function.braces false
                ]

        SReturn e ->
            List.concat
                [ ws cl.return.keyword <| keywordToken "return"
                , wss cl.return.expr <| expression e
                , ws cl.return.semi <| semiToken
                ]

        SContinue ->
            List.concat
                [ ws cl.continue.keyword <| keywordToken "continue"
                , ws cl.continue.semi <| semiToken
                ]

        SBreak -> [keywordToken "break", semiToken]
        SGoTo l -> [keywordToken "goto", labelToken l, semiToken]
        SLabel l -> [keywordToken "label", labelToken (l ++ ":"), semiToken]



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
whiteSpaceToken = Token "whiteSpace"

space = whiteSpaceToken " "


tokens : List Token -> Line
tokens ts =
    Table [ts]
