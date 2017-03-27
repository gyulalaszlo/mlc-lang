module Codegen.Lang exposing (..)


type CType
    = Void
    | Integral String
    | Structure String
    | StaticArray Int CType
    | Pointer CType
    | Const CType

type alias Identifier =
    String

type alias TypeIdentifier =
    CType

type BinaryOperator
    = OpIntPlus
    | OpIntMinus
    | OpIntMultiply
    | OpIntDivision
    | OpIntModulo
    | OpIntLShift
    | OpIntRShift

    | OpCompEq
    | OpCompNeq
    | OpCompGt
    | OpCompGte
    | OpCompLt
    | OpCompLte

    | OpBoolAnd
    | OpBoolOr
    | OpBoolXor

    | OpAssign


type Expression
    = IntegerLiteral String
    | ValueOf Identifier
    | BinaryExpression BinaryOperator Expression Expression


type Statement
    = DeclareLocal TypeIdentifier Identifier
    | ExpressionStatement Expression
    | Return Expression
    | IfStatement Expression (List Statement)
    -- Keep it primitive -- no statements in init
    | ForLoop Expression Expression Expression Statements
    | WhileLoop Expression Statements
    | LabelDeclaration String




type alias Statements = List Statement