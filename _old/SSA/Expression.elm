module SSA.Expression exposing (..)

import Dict exposing (Dict)
import SSA.Types exposing (SymbolType(..))
import Set exposing (Set)


-- SSA Expressions
-- ===============

type alias SymbolName = String
type alias LabelName = String

{-| A symbol is a combination of its name and its type.

|-}
type alias Sym =
    { name: SymbolName
    , type_: SymbolType
    }


type alias Assign =
    { to: SymbolName
    , op: SymbolName
    , args: List SymbolName
    }

type alias AssignListMember =
    { label: LabelName
    , body: List Assign
    }

type alias AssignList = List AssignListMember


{-| Expressions are the basic blocks of the AST
|-}
type alias Expression =
    { parameters: List Sym
    , returnType: SymbolType

    , head: LabelName
    , tails: Set LabelName

    , locals: List Sym
    , bodies: Dict LabelName (List Assign)
    }


empty =
    { parameters = []
    , returnType = Void
    , head = ""
    , tails = Set.fromList []

    , locals = []
    , bodies = List Dict.empty
    }


{-

    Returns a new blank expression that has no arguments and returns void.

-}
noOp : Expression
noOp =
    { parameters = []
    , returnType = Void
    , head = "noop"
    , tails = Set.fromList ["noop"]

    , locals = []
    , bodies = Dict.fromList [("noop", [])]
    }


{-
    Creates a 1-arity function that returns the input.
-}
identity : SymbolType -> Expression
identity t =
    { empty
        | parameters = [Sym "i" t]
        , returnType = t
        , head = "ret"
        , tails = Set.fromList ["ret"]
        , bodies = Dict.fromList
            [ ("ret", [Assign "_return" "alias" ["i"]])
            ]
        }


{-
    Creates a 1-arity function that returns the input.
-}
const : SymbolType -> String -> Expression
const t val =
    let
        name = "const:" ++ val
    in
        { empty
            | parameters = []
            , returnType = t
            , head = name
            , tails = Set.fromList [name]
            , bodies = Dict.fromList
                [ (name, [Assign "_return" "constFrom" ["CONSTS." ++ val]])
                ]
            }

{-

-}
--concat : Expression -> Expression -> Expression
--concat a b =
--



--{-
--    Creates a 1-arity function that returns the input.
---}
--identity : SymbolType -> Expression
--identity t =
--    { empty
--        | parameters = [Sym "i" t]
--        , returnType = t
--        , head = "ret"
--        , tails = Set.fromList ["ret"]
--        , bodies = Dict.fromList
--            [ ("ret", [Assignment "_return" "alias" ["i"]])
--            ]
--        }
{-
    Adds the result of a expression as an assignment.

    1. rewrite the returns of the child expression tree to return to a new label
    2. That label should assign the result expression value

    3. Put that label into the flow of the parent expression
-}
--
--appendToLeaf : LabelName -> Expression  -> Expression -> Expression
--appendToLeaf l child parent =


assignToExprResult : Sym -> Expression -> label -> Expression
assignToExprResult s e =
    let
        addToAssignments : Sym -> Expression -> Maybe (List Assign) -> Maybe (List Assign)
        addToAssignments s =
            Maybe.map <|
                \a -> List.append a [Assign s.name  ]

        expr = { empty
                    | parameters = []
                    , returnType = s.typ_
                    , head = "head"
                    , tails = Set.fromList ["head"]
                    , bodies = Dict.empty
                    , locals = []
                    }
    in
        expr




