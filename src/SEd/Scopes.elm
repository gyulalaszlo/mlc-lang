module SEd.Scopes exposing
    ( BasicScope(..)
    , Path
    , ScopeTraits
    , leafScopeTraits

    , StringScopeTraits
    , ListScopeTraits

    , listScopeFor, childKeys
    , childScopeAt, childScopeAndTraitsAt, recursiveChildScopeAndTraitsAt
    , stepLeft, stepRight, stepUp, stepDown, stepIntoNth

    , OpResult, OpSuccess
    , opOk
    , opErr


    , ScopeLikeTraits
    , scopeTraitsFor,  replace, update, append, remove
    , reducePath, mapPath

    , appendableTypes
    , recursiveAppend, recursiveRemove, recursiveSetString
    )

{-| Describe me please...
-}

import Error exposing (Error)
import List.Extra
import Set


type BasicScope scopeKey scope childKey
    = StringScope (StringScopeTraits scope)
    | ListScope (ListScopeTraits scopeKey scope childKey)





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


-- CURSOR ----------------------------------------------------------------------

type Cursor childKey
    = Leaf
    | Child childKey (Cursor childKey)



-- SCOPE TRAITS ----------------------------------------------------------------


{-| All scopes need these traits
-}
type alias ScopeTraits scopeKey scope childKey data =
    { fromData: (data -> Maybe scope)
    , toData: (scope -> Maybe data)

    , toLabel: (scope -> String)

    , base: BasicScope scopeKey scope childKey
    }


{-|
} Scope traits for string-based scopes
-}
type alias StringScopeTraits scope =
    { toString : scope -> String
    , fromString : String -> Result Error scope
    }

{-| List scope
-}
type alias ListScopeTraits scopeKey scope childKey =
    { childKeys: scope -> Maybe (List childKey)
    , childKindsAt: childKey -> scope -> Maybe (PossibleScopes scopeKey)
    , childScopeAt: childKey -> scope -> Maybe scope

    , appendableTypes: scope -> List scopeKey
    , append: scope -> scope -> OpResult scope childKey
    , replace: childKey -> scope -> scope -> OpResult scope childKey
    , remove: childKey -> scope -> OpResult scope childKey

    }


nada _ _ = Nothing


leafScopeTraits : ScopeTraits k s i d
leafScopeTraits =
    { toData = always Nothing
    , fromData = always Nothing
    , toLabel = toString
    , base = StringScope
        { toString = toString
        , fromString = always (Error.err "No setString() implementation defined")
        }
--    , steps = {}
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

-- STRING SCOPES ---------------------------------------------------------------

setString : ScopeLikeTraits k s i d -> String -> s -> OpResult s i
setString traits str s =
    let scopeTraits = scopeTraitsFor traits s
    in case scopeTraits.base of
        StringScope ts ->
            ts.fromString str
                |> Result.map (\ss -> { cursor = [], new = ss })
        _ ->
            Error.errMsg ["Cannot setString for non-string scope"
            , toString s ]


recursiveSetString : ScopeLikeTraits k s i d -> String -> Path i -> s -> OpResult s i
recursiveSetString traits str path s =
    case path of
        [] -> setString traits str s
        i :: is -> update traits i (recursiveSetString traits str is) s

-- LIST SCOPES -----------------------------------------------------------------


{-| Wraps ListScopeTraits.replace
-}
replace : ScopeLikeTraits k s i d -> i -> s -> s -> OpResult s i
replace traits i new s =
    listScopeFor traits s
        |> Result.andThen (\listTraits -> listTraits.replace i new s)


{-| Wraps ListScopeTraits.append
-}
append : ScopeLikeTraits k s i d -> s -> s -> OpResult s i
append traits new s =
    listScopeFor traits s
        |> Result.andThen (\listTraits -> listTraits.append new s)


{-| Wraps ListScopeTraits.appendableTypes
-}
appendableTypes : ScopeLikeTraits k s i d -> s -> Result Error (List k)
appendableTypes traits s =
    listScopeFor traits s
        |> Result.map (\listTraits -> listTraits.appendableTypes s)


{-| Wraps ListScopeTraits.remove
-}
remove : ScopeLikeTraits k s i d -> i -> s -> OpResult s i
remove traits i s =
    listScopeFor traits s
        |> Result.andThen (\listTraits -> listTraits.remove i s)

{-| Recursive update wrapper.
-}
update : ScopeLikeTraits k s i d -> i -> (s -> OpResult s i) -> s -> OpResult s i
update traits i fn s =
    let ts = scopeTraitsFor traits s
        err = Error.makeMsg ["Cannot find child at", toString i]

        concatCursor cursor res =
             { res | cursor = res.cursor ++ cursor }
    in
        childScopeAt traits i s
            |> Result.andThen fn
            |> Result.andThen (\child ->
                replace traits i child.new s
                    |> Result.map (concatCursor child.cursor))


-- RECURSIVE -------------------------------------------------------------------

recursiveAppend : ScopeLikeTraits k s i d -> Path i -> s -> s -> OpResult s i
recursiveAppend traits path new s =
    case path of
        [] -> append traits new s
        i :: is -> update traits i (recursiveAppend traits is new) s



recursiveRemove : ScopeLikeTraits k s i d -> Path i -> s -> OpResult s i
recursiveRemove traits path s =
    case path of
        [] -> Error.err "Cannot remove the root"
        [i] -> remove traits i s
        i :: is -> update traits i (recursiveRemove traits is) s


-- SCOPE QUICK ACCESS ----------------------------------------------------------


{-| Returns the list scope traits for the current scope or Error if the current scope
is not a list scope
-}
listScopeFor : ScopeLikeTraits k s i d -> s -> Result Error (ListScopeTraits k s i)
listScopeFor traits s =
    let ts = scopeTraitsFor traits s
    in case ts.base of
        ListScope listTraits -> Ok listTraits
        _ -> Error.errMsg ["Not a list scope:", toString s]


childScopeAt : ScopeLikeTraits k s i d -> i -> s -> Result Error s
childScopeAt traits i s =
    let err i = (Error.makeMsg ["Cannot find child at:", toString i])
    in listScopeFor traits s
        |> Result.andThen (\ts ->
            ts.childScopeAt i s |> Result.fromMaybe (err i))


childScopeAndTraitsAt : ScopeLikeTraits k s i d -> i -> s -> Result Error (ScopeTraits k s i d, s)
childScopeAndTraitsAt traits i s =
    let err i = (Error.makeMsg ["Cannot find child at:", toString i])
    in listScopeFor traits s
        |> Result.andThen (\ts ->
            ts.childScopeAt i s
                |> Result.fromMaybe (err i)
                |> Result.map (\cs -> (scopeTraitsFor traits cs, cs)))


childKeys : ScopeLikeTraits k s i d -> s -> Result Error (List i)
childKeys traits s =
    let err s = (Error.makeMsg ["Cannot find child Keys for:", toString s])
    in listScopeFor traits s
        |> Result.andThen (\ts ->
            ts.childKeys s |> Result.fromMaybe (err s))


{-| Returns a child scope by path
-}
recursiveChildScopeAt : ScopeLikeTraits k s i d -> Path i -> s -> Result Error s
recursiveChildScopeAt traits path s =
    case path of
        [] -> Ok s
        i :: is ->
            childScopeAt traits i s
                |> Result.andThen (recursiveChildScopeAt traits is)


{-| Returns a child scope and its traits by path
-}
recursiveChildScopeAndTraitsAt : ScopeLikeTraits k s i d -> Path i -> s -> Result Error (ScopeTraits k s i d, s)
recursiveChildScopeAndTraitsAt traits path s =
    case path of
        [] -> Ok (scopeTraitsFor traits s, s)
        i :: is ->
            childScopeAt traits i s
                |> Result.andThen (recursiveChildScopeAndTraitsAt traits is)

-- STEPS -----------------------------------------------------------------------

{-| Base implementation for remapping a cursor position in the childKeys list of a path
-}
stepLR : ScopeLikeTraits k s i d -> (Int -> Int) -> Path i -> s -> Result Error (Path i)
stepLR traits keyModFn path s =
    case path of
        [] -> Ok []
        [i] -> childKeys traits s
                |> Result.map (\ks ->
                        List.Extra.elemIndex i ks
                            |> Maybe.map keyModFn
                            |> Maybe.andThen (\i -> List.head <| List.drop i ks)
                            |> Maybe.withDefault i
                            |> List.singleton)
        i :: is ->
            childScopeAt traits i s
                |> Result.andThen (stepLR traits keyModFn is)
                |> Result.map (\ps -> i :: ps)

stepLeft : ScopeLikeTraits k s i d -> Path i -> s -> Result Error (Path i)
stepLeft traits path s =
    stepLR traits (\i -> i - 1) path s

stepRight : ScopeLikeTraits k s i d -> Path i -> s -> Result Error (Path i)
stepRight traits path s =
    stepLR traits (\i -> i + 1) path s


stepUp : Path i -> Result Error (Path i)
stepUp path =
    case path of
        [] -> Error.err "Cannot step up in empty path, it already points to root."
        [i] -> Ok []
        i :: is -> stepUp is |> Result.map (\newPath -> i :: newPath)



stepDown : ScopeLikeTraits k s i d -> Path i -> s -> Result Error (Path i)
stepDown traits path s =
    case path of
        [] -> childKeys traits s
                |> Result.map (\ks -> List.take 1 ks)
        i :: is -> childScopeAt traits i s
                |> Result.andThen (stepDown traits is)
                |> Result.map (\ps -> i :: ps)


{-| Tries to step down into the i-th child of the scope. Returns the new path or Error
if an error occurred.
-}
stepIntoNth : ScopeLikeTraits k s i d -> Path i -> i -> s -> Result Error (Path i)
stepIntoNth traits path i s =
    case path of
        [] -> childKeys traits s
                |> Result.map (\ks -> if List.member i ks then [i] else [] )
        ii :: is -> childScopeAt traits ii s
                |> Result.andThen (stepIntoNth traits is i)
                |> Result.map (\ps -> ii :: ps)

--------------------------------------------------------------------------------


reducePath : ScopeLikeTraits k s i d -> (i -> ScopeTraits k s i d -> s -> a -> Result Error a) -> a -> Path i -> s -> Result Error a
reducePath traits fn init path s =
    case path of
        [] -> Ok init
        i :: is ->
            childScopeAndTraitsAt traits i s
                |> Result.andThen (\(ts,ss) -> fn i ts ss init |> Result.map (\v -> (v, ss)))
                |> Result.andThen (\(aa,ss) -> reducePath traits fn aa is ss)

{-| Steps through the path and returns the OK with a list with the return values for each
iteration, or an Error if any errors occurred along the way
-}
mapPath : ScopeLikeTraits k s i d -> (i -> ScopeTraits k s i d -> s -> Result Error a) -> Path i -> s -> Result Error (List a)
mapPath traits fn path s =
    reducePath traits
        (\i ts s ss -> (fn i ts s) |> Result.map (\v -> ss ++ [v]))
       []
       path s
