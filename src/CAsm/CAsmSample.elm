module CAsm.CAsmSample exposing (..)
{-| Sample for generating a simple expression block
|-}

import CAsm.CAsm exposing (..)
import CAsm.SymbolType exposing (..)
import CAsm.DSL exposing (..)
import CAsm.CPrinter exposing (toCCode)
import Codegen.Indented exposing (applyIndents)
import Html

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
        const "0" <|
        const "1" <|
        const "hello"

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
            , let_ next (builtin "U8" "plus", [i, c1])
            , branch match.name "loop.done" "loop.pre"
            ]


        <| block "loop.done"
            [ phi "loop.iter" retval (builtin "Maybe_U64" "just", [i])
            , phi "loop.pre" retval (builtin "Maybe_U64" "nothing", [])
            , return retval.name
            ]

        <| withNameAndParams "indexOfStr" [ch, s]


main =
    Html.div []
        [ Html.pre [] [ Html.text <| String.join "\n" <| applyIndents <| toCCode sample ]
        , Html.hr [] []
        , Html.pre [] [ Html.text <| prettyPrint sample ]
        , Html.hr [] []
        , Html.code [] [ Html.text <| toString sample ]
        ]
