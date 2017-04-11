module SEd.Scopes.SExprScopes exposing (..)

{-| Describe me please...
-}

import List.Extra
import SEd.Scopes exposing (..)


{-| test scope
-}
type Scope
    = EKey String
    | EList (List Scope)


type SExprScopeType
    = SKeyScope
    | SListScope


type alias SExprData =
    String


type alias SExprScopeTraits =
    ScopeTraits SExprScopeType Scope Int SExprData



----------


kindOf : Scope -> SExprScopeType
kindOf scope =
    case scope of
        EList _ ->
            SListScope

        EKey _ ->
            SKeyScope


traitsFor : SExprScopeType -> SExprScopeTraits
traitsFor s =
    case s of
        SKeyScope ->
            keyTraits

        SListScope ->
            listTraits


empty : SExprScopeType -> Scope
empty k =
    case k of
        SKeyScope ->
            EKey ""

        SListScope ->
            EList []



-- KEY TRAITS -----------------------------------------

keyTraits : SExprScopeTraits
keyTraits =
    { leafScopeTraits
        | base = StringScope
        , operationSupports = (\_ _ -> Just [ SKeyScope ])
        , toData = Just << recursiveToData
        , childDataAt = keyChildData
    }


{-| keyChildData
-}
keyChildData : Int -> Scope -> Maybe SExprData
keyChildData i s =
    case s of
        EKey s ->
            Just s

        _ ->
            Nothing


-- LIST TRAITS -----------------------------------------


listTraits : SExprScopeTraits
listTraits =
    { base = ListScope
    , toData = Just << recursiveToData
    , operationSupports = listChildOperationSupports
    , childKeys = listChildKeys
    , childKindsAt = listChildKinds
    , childDataAt = listChildData
    , childScopeAt = listChildScopeFor
    , stepLeft = listStepLeft
    , stepRight = listStepRight
    , operateOnChildAt = listOperateOnChildAt
    , appendableTypes = listAppendableTypes
    , append = listAppend
    , replace = listReplace
    }


listChildKinds : Int -> Scope -> Maybe (List SExprScopeType)
listChildKinds i s =
    listChildKindsData


listChildKindsData =
    Just
        [ SKeyScope
        , SListScope
        ]


{-| list Child Data
-}
listChildData : Int -> Scope -> Maybe SExprData
listChildData i s =
    case s of
        EList es ->
            List.Extra.getAt i es
                |> Maybe.map recursiveToData

        _ ->
            Nothing


{-| list Child Count
-}
listChildKeys : Scope -> Maybe (List Int)
listChildKeys s =
    case s of
        EList es ->
            Just <| List.range 0 (List.length es - 1)

        _ ->
            Nothing


{-| list Child Scope For
-}
listChildScopeFor : Int -> Scope -> Maybe Scope
listChildScopeFor i s =
    case s of
        EList es ->
            List.Extra.getAt i es

        _ ->
            Nothing


listStepLeft : Int -> Scope -> Maybe Int
listStepLeft i s =
    if i > 0 then
        Just <| i - 1
    else
        Nothing


listStepRight : Int -> Scope -> Maybe Int
listStepRight i s =
    case s of
        EList es ->
            if (i + 1) < List.length es then
                Just (i + 1)
            else
                Nothing

        _ ->
            Nothing


listChildOperationSupports : BasicOperation Scope -> Scope -> Maybe (List SExprScopeType)
listChildOperationSupports op s =
    case op of
        AppendOperation _ ->
            listChildKindsData

        ReplaceOperation _ ->
            Just [ SListScope ]

        RemoveOperation ->
            Just [ SListScope ]


{-| recursive To Data
-}
recursiveToData : Scope -> SExprData
recursiveToData a =
    case a of
        EList es ->
            "(" ++ String.join " " (List.map recursiveToData es) ++ ")"

        EKey s ->
            ":" ++ s


listOperateOnChildAt: BasicOperation Scope -> Int -> Scope -> Maybe (Maybe Int, Scope)
listOperateOnChildAt op k s =
    case (s, op) of
        (EList es, RemoveOperation) ->
            let new = List.Extra.removeAt k es
            in case new of
                [] -> Just (Nothing, EList [])
                _ -> Just (Just <| max 0 (k - 1), EList new)

        (EList es, ReplaceOperation el) ->
            List.Extra.setAt k el es
                |> Maybe.map (\es -> (Just k, EList es))

        (EList es, AppendOperation el) ->
            List.Extra.setAt k el es
                |> Maybe.map (\es -> (Just k, EList es))
        _ -> Nothing





-------------------------------------

listAppendableTypes: Scope -> List SExprScopeType
listAppendableTypes scope =
    [SKeyScope, SListScope]



listAppend: Scope -> Scope -> Maybe (Int, Scope)
listAppend new scope =
    case scope of
        EList es -> Just <| (List.length es, EList <| es ++ [ new ])
        _ -> Nothing

listReplace: Int -> Scope -> Scope -> Maybe Scope
listReplace i new scope =
    case scope of
        EList es ->
            List.Extra.setAt i new es
                |> Maybe.map (\es -> EList es)
        _ -> Nothing
