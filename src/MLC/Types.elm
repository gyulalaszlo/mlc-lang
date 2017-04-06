module MLC.Types exposing (..)
{-| Describe me please...
|-}

import Char
import List.Extra
import Set


type Expression
    = EList (List Expression)
--    | EDict (List (Expression, Expression))
--    | EVector (List Expression)
--
--    | EString String
--    | EChar String
--    | EIntegerLiteral String
--    | EFloatLiteral String
--
--    -- Scoped, can be exported-imported
--    | ESymbol SymbolData

    -- Unscoped, only means the name
    | EKey String



type alias SymbolData =
    { from: String
    , name: String
    }

