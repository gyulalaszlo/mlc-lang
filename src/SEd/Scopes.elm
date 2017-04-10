module SEd.Scopes exposing
    ( BasicScope(..)
    , BasicOperation(..)
    , ScopeTraits
    , leafScopeTraits
    , allOperations
    )

{-| Describe me please...
-}

import Set
type BasicScope
    = StringScope
    | IntScope
    | RecordScope
    | ListScope


type BasicOperation
    = ReplaceOperation
    | RemoveOperation
    | AppendOperation


{-| A list of all possible operations for a scope.
-}
allOperations =
    [ AppendOperation
    , ReplaceOperation
    , RemoveOperation
    ]



type alias PossibleScopes scopeKey
    = List scopeKey


{-| Performs a basic operation on a child and returns Just the updated version of
scope if the operation succeeded and Nothing otherwise.
-}
type alias ChildOperation scope childKey = BasicOperation -> childKey -> scope -> Maybe scope


{-| Groups behaviours for a scope
-}
type alias ScopeTraits scopeKey scope childKey data =
    { base: BasicScope
    , toData: (scope -> Maybe data)
    , operationSupports: BasicOperation -> scope -> Maybe (PossibleScopes scopeKey)


    , childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (PossibleScopes scopeKey)
    , childDataAt: childKey -> scope -> Maybe data
    , childScopeAt: childKey -> scope -> Maybe scope

    , stepLeft: childKey -> scope -> Maybe childKey
    , stepRight: childKey -> scope -> Maybe childKey

    , operateOnChildAt: BasicOperation -> Maybe scope -> childKey -> scope -> Maybe (Maybe childKey, scope)
    }


nada _ _ = Nothing

leafScopeTraits =
    { base = StringScope
    , toData = always Nothing
    , operationSupports = nada

    , childKeys = always Nothing
    , childKindsAt = nada
    , childDataAt = nada
    , childScopeAt = nada

    , stepLeft = nada
    , stepRight = nada

    , operateOnChildAt = (\_ _ _ _ -> Nothing)
    }

