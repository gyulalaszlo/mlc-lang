module CAsm.CAsmSample exposing (..)
{-| Sample for generating a sinple expression block
|-}

import CAsm.CAsm exposing (..)
import CAsm.SymbolType exposing (..)
import Html

sample =
    let
        i = Sym "i" u64
        next = Sym "next" u64
        l = Sym "l" u64
        s = Sym "s" str
        ch = Sym "ch" char

        c0 = Sym "__0" u64
        cmp = Sym "cmp" bool
    in
        const "0" <|
        const "1" <|
        const "hello" <|
        (let_ l ("C/strlen", [s])
            (block "loop.pre"
                (phi "main" i ("U64/from-const", [c0])
                (phi "loop.iter" i ("U64/alias", [next])
                    (let_ cmp ("U64/lt", [i, l])
                        empty)))))


main =
    Html.div []
        [ Html.pre [] [ Html.text <| prettyPrint sample ]
        , Html.hr [] []
        , Html.code [] [ Html.text <| toString sample ]
        ]
