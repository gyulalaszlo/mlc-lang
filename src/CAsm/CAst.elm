module CAst exposing (..)
{-| Describe me please...
|-}

import CAsm.CAsm exposing (..)


type LValue
    = Symbol Sym



type RValue
    = FromLValue LValue
    | FunctionCall FunctionName (List SymbolName)



type CExpr
    = ExprAssign LValue RValue
    | ExprDeclare Sym
    | Label String


type Statement
    = Expr CExpr
    | Labeled String (List Statement)
