module SEd.Scopes.Model exposing (..)

{-| Describe me please...
-}

import Error exposing (Error)
import SEd.ErrorView
import SEd.Scopes exposing (BasicScope(..), OpResult, OpSuccess, ScopeLikeTraits, ScopeTraits)
import SEd.Scopes.Msg exposing (Msg(SEdErrorViewMsg))


-- MODEL



type alias Model kind scope childKey data =
    { data : scope
    , path :
        List childKey
        --(kind, childKey)
    , errors : SEd.ErrorView.Model
    , traits : ScopeLikeTraits kind scope childKey data
    }


from : ScopeLikeTraits k s i d -> s -> Model k s i d
from traits scope =
    { data = scope
    , path = []
    , errors = SEd.ErrorView.initialModel
    , traits = traits
    }


{-| get the current scope at the models path
-}
currentScope : Model k s i d -> Maybe s
currentScope model =
    sExprAt model.traits model.path model.data


scopeAndTraitsForPath : List i -> Model k s i d -> Maybe ( s, ScopeTraits k s i d )
scopeAndTraitsForPath path model =
    let
        target =
            sExprAt model.traits path model.data
    in
        target
            |> Maybe.map model.traits.kindOf
            |> Maybe.map model.traits.traitsFor
            |> Maybe.map2 (,) target


sExprAt : ScopeLikeTraits k s i d -> List i -> s -> Maybe s
sExprAt scopeTraits path scope =
    case path of
        [] ->
            Just scope

        i :: path ->
            let
                kind =
                    scopeTraits.kindOf scope

                traits =
                    scopeTraits.traitsFor kind
            in
                case traits.base of
                    ListScope listTraits ->
                        listTraits.childScopeAt i scope
                            |> Maybe.andThen (sExprAt scopeTraits path)
                    StringScope _ -> Nothing


-- SUBSCRIPTIONS


subscriptions : Model k s i d -> Sub (Msg s i)
subscriptions model =
    Sub.batch
        [ Sub.map SEdErrorViewMsg <| SEd.ErrorView.subscriptions model.errors
        ]
