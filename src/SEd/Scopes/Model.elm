module SEd.Scopes.Model exposing (..)

{-| Describe me please...
-}

import SEd.Scopes exposing (ScopeTraits)
import SEd.Scopes.Msg exposing (Msg)


-- MODEL


{-| Traits
-}
type alias ScopeLikeTraits kind scope childKey data =
    { kindOf : scope -> kind
    , traitsFor : kind -> ScopeTraits kind scope childKey data
    }


type alias Model kind scope childKey data =
    { data : scope
    , path :
        List childKey
        --(kind, childKey)
    , traits : ScopeLikeTraits kind scope childKey data
    }


from : ScopeLikeTraits k s i d -> s -> Model k s i d
from traits scope =
    { data = scope
    , path = []
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
                traits.childScopeAt i scope
                    |> Maybe.andThen (sExprAt scopeTraits path)



-- SUBSCRIPTIONS


subscriptions : Model k s i d -> Sub (Msg i)
subscriptions model =
    Sub.none
