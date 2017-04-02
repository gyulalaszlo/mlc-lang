module CAst.CodeStyle exposing
    ( defaultCodeStyle, applyCodeStyle, CodeStyle, Ws, SideWs(..), IndentWs(..)

    )
{-| Describe me please...
|-}

import CAst.AstPrinter exposing (Token)
import Dict exposing (Dict)


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
        , ("while.paren.close", spaceRight)
        , ("while.braces.open", blockOpen)
        , ("while.braces.close", blockClose)

        , ("comment.block", breakAfter)
        , ("comment.block.line", breakAfter)
        , ("comment.block.open", blockOpen)
        , ("comment.block.close", blockClose)

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
        , ("if.paren.close", spaceRight)
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





type SideWs
    = NoSpace
    | Space
    | LineBreak

type IndentWs
    = Indent
    | Outdent
    | NoIndent

type alias Ws =
    { left: SideWs
    , right: SideWs
    , indentLeft: IndentWs
    , indentRight: IndentWs
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
outdentBefore = { emptyWs | indentLeft = Outdent}

outdentAfter : Ws
outdentAfter = { emptyWs |  indentRight = Outdent }

blockOpen = { emptyWs | right = LineBreak, indentRight = Indent }
blockClose = { emptyWs | indentLeft = Outdent, right = LineBreak }

type alias CodeStyle = Dict String Ws

whiteSpaceToken = Token "whiteSpace"
lineBreakToken = Token "line-break" "" "\n"
indentToken = Token "indent" "" ""
outdentToken = Token "outdent" "" ""


wss : String -> List Token -> List Token
wss w ts = List.map (\t -> { t | tag = w }) ts

applyWs : Ws -> Token -> List Token
applyWs {left, right} t =
    List.concat [sideWs t.tag left, [t], sideWs t.tag right]



applyCodeStyle : CodeStyle -> List Token -> List Token
applyCodeStyle cs ts =
    List.foldl (applyCodeStyleHelepr cs) [] ts
        |> List.foldl (applyIndentsFromTokens "\t") initialApplyIndentState
        |> .tokens
--    List.foldl (applyCodeStyleHelepr cs) (0,[]) ts
--        |> Tuple.second

applyCodeStyleHelepr : CodeStyle -> Token -> List Token -> List Token
applyCodeStyleHelepr cs t ts =
    let
        tokenWs = Dict.get t.tag cs |> Maybe.withDefault spaceRight


        wrapWithIndent : Ws -> List Token -> List Token
        wrapWithIndent tokenWs ts =
            List.concat
                [ tokensForIndent tokenWs.indentLeft
                , ts
                , tokensForIndent tokenWs.indentRight
                ]

    in
        ts ++ wrapWithIndent tokenWs (applyWs tokenWs t)

indentString : String
indentString = "\t"

type alias ApplyIndentState =
    { current: Int
    , needsIndent: Bool
    , tokens: List Token
    }

initialApplyIndentState : ApplyIndentState
initialApplyIndentState =
    { current = 0
    , needsIndent = True
    , tokens = []
    }

{-| Applies the indents coming from the white-space wrapping. This is done in two phases because
the indentation of the next line is unknown at whitespace-wrapping time.
-}
applyIndentsFromTokens : String -> Token -> ApplyIndentState -> ApplyIndentState
applyIndentsFromTokens indentWith t s =
    let indentStr n = whiteSpaceToken "indent" <| String.repeat n indentWith
        return indent tt = { s | current = indent, tokens = tokens ++ [tt]}

        setNeedsIndent ss = { ss | needsIndent = True }

        -- add indent to the
        withIndent i t ss = { ss | needsIndent = False, tokens = tokens ++ [indentStr i, t] }

        {tokens, current, needsIndent} = s
    in
        case t.class of
            "indent" -> return (current + 1) indentToken
            "outdent" -> return (max 0 (current - 1)) outdentToken
            "line-break" ->
                setNeedsIndent <|
                    return current lineBreakToken

            _ ->
                if needsIndent
                    then withIndent current t s
                    else return current t


{-
    Convert ws to tokens
-}

sideWs : String -> SideWs -> List Token
sideWs tag w =
    case w of
        NoSpace -> []
        Space -> [whiteSpaceToken tag " "]
        LineBreak -> [{ lineBreakToken | tag = tag }]



tokensForIndent : IndentWs -> List Token
tokensForIndent ind =
    case ind of
        Indent -> [indentToken]
        Outdent -> [outdentToken]
        NoIndent -> []

