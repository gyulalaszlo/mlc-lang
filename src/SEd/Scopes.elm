module SEd.Scopes exposing (..)
{-| Describe me please...
-}
type BasicScope
    = StringScope
    | IntScope
    | RecordScope
    | ListScope







type alias ScopeTraits scopeKey scope childKey data =
    { base: BasicScope
    , childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (List (String, scopeKey))
    , childDataAt: childKey -> scope -> Maybe data
    , childScopeAt: childKey -> scope -> Maybe scope

    , stepLeft: childKey -> scope -> Maybe childKey
    , stepRight: childKey -> scope -> Maybe childKey
    }


leafScopeTraits =
    { base = StringScope
    , childKeys = always Nothing
    , childKindsAt = (\_ _ -> Nothing)
    , childDataAt = (\_ _ -> Nothing)
    , childScopeAt = (\_ _ -> Nothing)

    , stepLeft = (\_ _ -> Nothing)
    , stepRight = (\_ _ -> Nothing)
    }

