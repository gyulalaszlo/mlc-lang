module SEd.Scopes exposing
    ( BasicScope(..)
    , BasicOperation(..)
    , ScopeTraits
    , leafScopeTraits
    )

{-| Describe me please...
-}

import Set
type BasicScope
    = StringScope
    | IntScope
    | RecordScope
    | ListScope


type BasicOperation scope
    = ReplaceOperation scope
    | RemoveOperation
    | AppendOperation scope




type alias PossibleScopes scopeKey
    = List scopeKey


{-| Performs a basic operation on a child and returns Just the updated version of
scope if the operation succeeded and Nothing otherwise.
-}
type alias ChildOperation scope childKey = BasicOperation scope -> childKey -> scope -> Maybe scope


{-| Groups behaviours for a scope
-}
type alias ScopeTraits scopeKey scope childKey data =
    { base: BasicScope
    , toData: (scope -> Maybe data)
    , operationSupports: BasicOperation scope -> scope -> Maybe (PossibleScopes scopeKey)


    , childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (PossibleScopes scopeKey)
    , childDataAt: childKey -> scope -> Maybe data
    , childScopeAt: childKey -> scope -> Maybe scope

    , stepLeft: childKey -> scope -> Maybe childKey
    , stepRight: childKey -> scope -> Maybe childKey

    , operateOnChildAt: BasicOperation scope -> childKey -> scope -> Maybe (Maybe childKey, scope)

    , appendableTypes: scope -> List scopeKey
    , append: scope -> scope -> Maybe (childKey, scope)
    , replace: childKey -> scope -> scope -> Maybe scope
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

    , operateOnChildAt = (\_ _ _ -> Nothing)
    , appendableTypes = always []
    , append = (\_ _ -> Nothing)
    , replace = (\_ new _ -> Just new)
    }

