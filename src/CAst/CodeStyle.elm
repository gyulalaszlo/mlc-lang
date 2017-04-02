module CAst.CodeStyle exposing
    ( defaultCodeStyle, applyCodeStyle

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

whiteSpaceToken = Token "whiteSpace"
lineBreakToken = Token "line-break" "" "\n"
indentToken = Token "indent" "" ""
outdentToken = Token "outdent" "" ""


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


