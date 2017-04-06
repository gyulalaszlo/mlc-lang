module SEd.Operations exposing (..)
{-| Defines possible operations
-}


type Mode
    = Normal
    | Insert

type Operation cursor node
    = InsertNodeAt cursor node
    | ReplaceNodeAt cursor node
    | DeleteNodeAt cursor

    -- | MoveCursorTo cursor



type ScopeKind
    = LeafScope
    -- A scope with any children (so they support
    | NodeScope


type alias ScopeMeta =
    { displayName: String
    , kind: ScopeKind
    }


type alias StringConverter v =
    { from: (String -> Maybe v)
    , to: (v -> String)
    }


type alias ScopeTraits v =
    { meta: (v -> ScopeMeta)
--    , str: StringConverter
    }





{-




type alias EListScope = { elements: List M.Expression}
type alias EVectorScope = { elements: List M.Expression}
type alias EDictScope = { elements: List (M.Expression, M.Expression) }
type alias EKeyScope = { name: String }

type EScope
    = IsList EListScope
    | IsVector EVectorScope
    | IsDict EDictScope
    | IsKey EKeyScope




-- TRAITS

traitsFor : EScope -> ScopeTraits M.Expression
traitsFor e =
    case e of
        IsList _ -> listTraits
        IsKey _ -> keyTraits
        IsVector _ -> vectorTraits
        IsDict _ -> dictScope




-- VEC

vectorTraits =
    { meta = vectorMeta
    }


vectorMeta : M.Expression -> Maybe (ScopeMeta M.Expression)
vectorMeta e =
    case e of
        M.EVector es ->   { displayName: "Vector"
                        , kind: NodeScope
                        }
        _ -> Nothing


-- VEC

vectorTraits =
    { meta = vectorMeta
    }


vectorMeta : M.Expression -> Maybe (ScopeMeta M.Expression)
vectorMeta e =
    case e of
        M.EVector es ->   { displayName: "Vector"
                        , kind: NodeScope
                        }
        _ -> Nothing




-- LIST

listTraits =
    { meta = listMeta
    }


listMeta : M.Expression -> Maybe (ScopeMeta M.Expression)
listMeta e =
    case e of
        M.EList es ->   { displayName: "List"
                        , kind: NodeScope
                        }
        _ -> Nothing


-- KEY

keyTraits =
    { meta = keyMeta
    }


keyMeta : M.Expression -> Maybe (ScopeMeta M.Expression)
keyMeta e =
    case e of
        M.EKey s ->   { displayName: ":" ++ s
                      , kind: LeafScope
                      }
        _ -> Nothing







-}
