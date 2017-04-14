module SEd.Scopes exposing
    ( BasicScope(..)
    , Path
    , ScopeTraits
    , leafScopeTraits

    , OpResult, OpSuccess
    , opOk
    , opErr


    , ScopeLikeTraits
    , scopeTraitsFor, replace, update, append, remove
    )

{-| Describe me please...
-}

import Error exposing (Error)
import Set


type BasicScope
    = StringScope
    | IntScope
    | RecordScope
    | ListScope





type alias PossibleScopes scopeKey
    = List scopeKey




-- PATHS -----------------------------------------------------------------------

type alias Path i = List i

concatPaths : OpSuccess s i -> OpSuccess s i -> OpSuccess s i
concatPaths a b =
     { b | cursor = a.cursor ++ b.cursor }



-- OPERATIONS ------------------------------------------------------------------



type alias OpSuccess scope childKey =
    { cursor: List childKey
    , new: scope
    }

type alias OpResult scope childKey =
    Result Error (OpSuccess scope childKey)

opOk: List childKey -> scope -> OpResult scope childKey
opOk k s = Ok <| OpSuccess k s

opErr: Error -> OpResult scope childKey
opErr e = Err e


-- SCOPE TRAITS ----------------------------------------------------------------


{-| All scopes need these traits
-}
type alias GenericScopeTraits scope data =
    { fromData: (data -> scope)
    , toData: (scope -> data)

    }

{-| Scope traits for string-based scopes
-}
type alias StringScopeTraits scope =
    { toString : scope -> String
    , fromString : String -> scope
    }

{-| List scope
-}
type alias ListScopeTraits scopeKey scope childKey data =
    { childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (PossibleScopes scopeKey)
    , childDataAt: childKey -> scope -> Maybe data
    , childScopeAt: childKey -> scope -> Maybe scope

    , appendableTypes: scope -> List scopeKey
    -- appends a new scope to the end of the target
    , append: scope -> scope -> OpResult scope childKey
    -- replace a child of a scope with a new scope
    , replace: childKey -> scope -> scope -> OpResult scope childKey
    -- remove a child from a scope by key
    , remove: childKey -> scope -> OpResult scope childKey
    }



{-| Groups behaviours for a scope
-}
type alias ScopeTraits scopeKey scope childKey data =
    { base: BasicScope
    , toData: (scope -> Maybe data)

    , childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (PossibleScopes scopeKey)
    , childDataAt: childKey -> scope -> Maybe data
    , childScopeAt: childKey -> scope -> Maybe scope

    , stepLeft: childKey -> scope -> Maybe childKey
    , stepRight: childKey -> scope -> Maybe childKey

    , appendableTypes: scope -> List scopeKey
    -- appends a new scope to the end of the target
    , append: scope -> scope -> OpResult scope childKey
    -- replace a child of a scope with a new scope
    , replace: childKey -> scope -> scope -> OpResult scope childKey
    -- remove a child from a scope by key
    , remove: childKey -> scope -> OpResult scope childKey
    }


nada _ _ = Nothing


leafScopeTraits : ScopeTraits k s i d
leafScopeTraits =
    { base = StringScope
    , toData = always Nothing

    , childKeys = always Nothing
    , childKindsAt = nada
    , childDataAt = nada
    , childScopeAt = nada

    , stepLeft = nada
    , stepRight = nada

--    , operateOnChildAt = (\_ _ _ -> Nothing)
    , appendableTypes = always []
    , append = (\_ _ -> Error.err "Append not defined for this node type.")
    , replace = (\i new _ -> opOk [i] new)
    , remove = (\_ _ -> Error.err "Remove not defined for this node type.")
    }


-- SCOPE TREES -----------------------------------------------------------------


{-| Traits
-}
type alias ScopeLikeTraits kind scope childKey data =
    { kindOf : scope -> kind
    , traitsFor : kind -> ScopeTraits kind scope childKey data
    , empty: kind -> scope
    }


{-| Gets the scope traits for a scope

    traits.traitsFor <| traits.kindOf scope
-}
scopeTraitsFor : ScopeLikeTraits k s i d -> s -> ScopeTraits k s i d
scopeTraitsFor traits scope =
    traits.traitsFor <| traits.kindOf scope



replace : ScopeLikeTraits k s i d -> i -> s -> s -> OpResult s i
replace traits i new s =
    let ts = scopeTraitsFor traits s
    in ts.replace i new s

append : ScopeLikeTraits k s i d -> s -> s -> OpResult s i
append traits new s =
    let ts = scopeTraitsFor traits s
    in ts.append new s

remove : ScopeLikeTraits k s i d -> i -> s -> OpResult s i
remove traits i s =
    let ts = scopeTraitsFor traits s
    in ts.remove i s

update : ScopeLikeTraits k s i d -> i -> (s -> OpResult s i) -> s -> OpResult s i
update traits i fn s =
    let ts = scopeTraitsFor traits s
        err = Error.makeMsg ["Cannot find child at", toString i]

        concatCursor child res =
             { res | cursor = res.cursor ++ child.cursor }
    in
        ts.childScopeAt i s
            |> Result.fromMaybe (err)
            |> Result.andThen fn
            |> Result.andThen (\child ->
                ts.replace i child.new s
                    |> Result.map (concatCursor child))


