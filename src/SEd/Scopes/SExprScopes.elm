module SEd.Scopes.SExprScopes exposing (..)

{-| Describe me please...
-}

import Error
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
        , toData = Just << recursiveToData
    }



-- LIST TRAITS -----------------------------------------


listTraits : SExprScopeTraits
listTraits =
    { base = ListScope
    , toData = Just << recursiveToData
    , childKeys = listChildKeys
    , childKindsAt = listChildKinds
    , childDataAt = listChildData
    , childScopeAt = listChildScopeFor
    , stepLeft = listStepLeft
    , stepRight = listStepRight
    , appendableTypes = listAppendableTypes
    , append = listAppend
    , replace = listReplace
    , remove = listRemove
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




-------------------------------------

listAppendableTypes: Scope -> List SExprScopeType
listAppendableTypes scope =
    [SKeyScope, SListScope]



listAppend: Scope -> Scope -> OpResult Scope Int
listAppend new scope =
    case scope of
        EList es -> opOk [List.length es] (EList <| es ++ [ new ])
        _ -> Error.err "Type error."


listReplace: Int -> Scope -> Scope -> OpResult Scope Int
listReplace i new scope =
    let err es =
            Error.errMsg
                ["Cannot find child at"
                , toString i , "in", toString es]
    in case scope of
        EList es ->
            List.Extra.setAt i new es
                |> Maybe.map (opOk [i] << EList)
                |> Maybe.withDefault (err es)
        _ -> Error.err "Type error."


listRemove : Int -> Scope -> OpResult Scope Int
listRemove i s =
    case s of
        EList es ->
            let new = List.Extra.removeAt i es
            in case new of
                [] -> opOk [] <| EList []
                _ -> opOk [max 0 (i - 1)] <| EList new
        _ -> Error.err "Type error."




-- RECURSIVE TO DATA -----------------------------------


{-| recursive To Data
-}
recursiveToData : Scope -> SExprData
recursiveToData a =
    case a of
        EList es ->
            "(" ++ String.join " " (List.map recursiveToData es) ++ ")"

        EKey s ->
            s
--            ":" ++ s

