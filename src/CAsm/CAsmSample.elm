module CAsm.CAsmSample exposing (..)
{-| Sample for generating a simple expression block
|-}

import CAsm.AstBuilder exposing (toAst, toFunction)
import CAsm.AstPrinter exposing (functionToString)
import CAsm.CodeStyle exposing (defaultCodeStyle, applyCodeStyle)
import CAsm.CAsm exposing (..)
import CAsm.Error exposing (errorToString)
import CAsm.SymbolType exposing (..)
import CAsm.DSL exposing (..)
import Html
import Html.Attributes exposing (attribute, class)

sample =
    let
        i = Sym "i" u64 LValue
        next = Sym "next" u64 RValue
        l = Sym "l" u64 LValue
        s = Sym "s" str LValue
        ch = Sym "ch" char LValue
        current = Sym "current" char RValue
        match = Sym "match" char RValue

        c0 = Sym "__0" u64 RValue
        c1 = Sym "__1" u64 RValue
        cmp = Sym "cmp" bool RValue

        retval = Sym "retval" (Parametric <| ParametricType "Maybe" [u64])  RValue
    in
        const "1" <|
        const "0"

        <| block "main"
            [ let_ l (cFn "strlen", [s])
            ]

        <| block "loop.pre"
            [ phi "main" i (builtin "U64" "from-const", [c0])
            , phi "loop.iter" i (builtin "U64" "alias", [next])
            , let_ cmp (builtin "U64" "lt", [i, l])
            , branch cmp.name "loop.iter" "loop.done"
            ]

        <| block "loop.iter"
            [ let_ current (builtin "U8" "at", [s, i])
            , let_ match (builtin "U8" "eq", [current, ch])
            , let_ c1 (builtin "U64" "from-const", [c1])
            , let_ next (builtin "U64" "plus", [i, c1])
            , branch match.name "loop.done" "loop.pre"
            ]


        <| block "loop.done"
            [ phi "loop.iter" retval (builtin "Maybe_U64" "just", [i])
            , phi "loop.pre" retval (builtin "Maybe_U64" "nothing", [])
            , return retval.name
            ]

        <| withNameAndParams "indexOfStr" [ch, s] (Parametric (ParametricType "Maybe" [u64]))



tokenList ts =
    List.map
        (\{class, text, tag} -> Html.span
            [Html.Attributes.class ("token token-" ++ class ++ " " ++ class)
            , attribute "data-tag" tag
            , attribute "data-class" class
            ]
            [Html.text text] )
        ts

astView s =
    case toAst s of
        Ok ast -> Html.code []
                [ Html.pre [class "c-code"]  <| tokenList <| applyCodeStyle defaultCodeStyle <| functionToString <| toFunction s ast
                , Html.hr [] []
--                , Html.text <| toString ast
                ]
        Err errors -> Html.pre [] [ Html.text <| errorToString errors]


main =
    Html.div []
        [ css
        , astView sample
        , Html.hr [] []
        , Html.pre [] [ Html.text <| prettyPrint sample ]
        ]

css = Html.node "style" [Html.Attributes.type_ "text/css"]
          [Html.text """

body, pre {  font-family: "Fira Code", Monaco, Courier New; font-size: 12px; }

pre { line-heigth:1.2em; }


.c-code { padding: 2em 0.5em; white-space: pre-wrap; background: #222220; color: #999; }

.c-code .token { cursor: pointer; border:none; }
.c-code .token:hover { background: #444; border-radius: 2px; }
.c-code .token-keyword { color: #c33; }
.c-code .token-type { color: #39c; }
.c-code .token-symbol { color: #7c7; }
.c-code .token-comment { color: #666; }
.c-code .token-parenthesis,
.c-code .token-colon,
.c-code .token-semicolon { color: #444; }
.c-code .token-op,
.c-code .token-assign { color: #c90; }
.c-code .token-literal { color: #c09; }
.c-code .token-functionName { color: #5ca; }

.c-code .token-line-break { color: #3c3c3c; }
.c-code .token-line-break:before { content: "⏎"; }

.c-code .token-whiteSpace {  background: #222;  }
.c-code .token-whiteSpace:hover {  background: #333;  }


.c-code .token-keyword:hover { background-color: #c33; color: #222; }
.c-code .token-type:hover { background-color: #39c; color: #222; }
.c-code .token-symbol:hover { background-color: #7c7; color: #222; }
.c-code .token-functionName:hover { background-color: #5ca; color: #222; }
                  """
                  ]
