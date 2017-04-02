module CAsm.CAst exposing (..)

{-| Describe me please...
-}

import CAsm.CAsm exposing (..)
import CAsm.SymbolType exposing (SymbolType)


type alias FunctionStatement =
    { name : String
    , args: List (SymbolName, SymbolType)
    , returns: SymbolType
    , body: StatementList
    }

type alias StatementList = List Statement

type Statement
    = SWhile WhileStatement
    | SIf IfStatement
    | SReturn Expression
    | SGoTo LabelName
    | SContinue
    | SBreak
    | SLabel LabelName
      -- LValues are special
    | SLValueDeclare LValueDeclareStatement
    | SLValueAssign LValueAssignStatement
    -- Comments are also special
    | SComment (List String)


{-| A while loop is a condition and a body wrapped
-}
type alias WhileStatement =
    { condition : Expression
    , body : StatementList
    }


{-| An if as a condition and two branches wrapped.
-}
type alias IfStatement =
    { condition : Expression
    , true : StatementList
    , false : StatementList
    }



{-| Wrapper for data about an Lvalue declaration
-}
type alias LValueDeclareStatement =
    { name : SymbolName
    , type_ : SymbolType
    }


{-| Wrapper for data about an Lvalue declaration
-}
type alias LValueAssignStatement =
    { name : SymbolName
    , type_ : SymbolType
    , value : Expression
    }


{-| An expression wraps its return type and the body of the expression
-}
type alias Expression =
    { meta : ExpressionMeta
    , body : ExpressionBody
    }


{-| Wraps metadata about an expression
-}
type alias ExpressionMeta =
    { returns : SymbolType
    }


{-| Contains the he actual details of the expression.
-}
type ExpressionBody
    = ExprBinary BinaryOp Expression Expression
    | ExprLValue SymbolName
    | ExprLiteral Literal
    | ExprFunctionCall SymbolName (List Expression)
    | ExprWithComment String ExpressionBody


{-| Value from a constant literal
-}
type alias Literal =
    { text : String
    }


{-| A binary expression
-}
type alias BinaryExpression =
    { operator : BinaryOp
    , left : Expression
    , right : Expression
    }


{-| Binary operators
-}
type BinaryOp
    = BinaryPlus
    | BinaryMinus
    | BinaryTimes
    | BinaryDividedBy
    | BinaryNth
    | CompLt
    | CompGt
    | CompEq
    | CompNeq


{-
    Statement factories
    -------------------
-}

{-| Factory for if statements
-}
makeIf : Expression -> StatementList -> StatementList -> Statement
makeIf e t f =
    SIf { condition = e, true = t, false = f }

{-| Factory for While statements
-}
makeWhile : Expression -> StatementList -> Statement
makeWhile e l =
    SWhile { condition = e, body = l }


makeLValuleDeclare : SymbolName -> SymbolType -> Statement
makeLValuleDeclare n t =
    SLValueDeclare { name = n, type_ = t }

makeLValueAssign : SymbolName -> SymbolType -> Expression -> Statement
makeLValueAssign n t e =
    SLValueAssign { name = n, type_ = t, value = e }

{-
    Expression factories
    --------------------
-}

makeExpression : SymbolType -> ExpressionBody -> Expression
makeExpression t b = { meta = { returns = t }, body = b }

makeLiteral : SymbolType -> String -> Expression
makeLiteral t v = makeExpression t (ExprLiteral { text = v })

makeComment : String -> Expression -> Expression
makeComment c es = { meta = es.meta, body =  ExprWithComment c es.body }

makeBinary : SymbolType -> BinaryOp -> Expression -> Expression -> Expression
makeBinary t o l r =
    { meta = { returns = t }, body = ExprBinary o l r}

makeLValueExpr : SymbolName -> SymbolType -> Expression
makeLValueExpr n t =
    { meta = { returns = t }, body = ExprLValue n }

