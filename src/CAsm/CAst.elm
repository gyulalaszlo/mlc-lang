module CAst exposing (..)

{-| Describe me please...
-}

import CAsm.CAsm exposing (..)
import CAsm.SymbolType exposing (SymbolType)


type alias StatementList =
    List Statement


type Statement
    = SWhile WhileStatement
    | SIf IfStatement
    | SReturn
    | SContinue
      -- LValues are special
    | SLValueDeclare LValueDeclareStatement
    | SLValueAssign LValueAssignStatement


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

makeLValuleAssign : SymbolName -> SymbolType -> Expression -> Statement
makeLValuleAssign n t e =
    SLValueDeclare { name = n, type_ = t, value = e }

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
    | ExprFunctionCall FunctionName (List Expression)


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


{-| Comparison operators
-}
type CompareOp
    = CompLt
    | CompGt
    | CompEq
    | CompNeq


type BinaryOp
    = BinaryPlus
    | BinaryMinus
    | BinaryTimes
    | BinaryDividedBy
    | BinaryNth
