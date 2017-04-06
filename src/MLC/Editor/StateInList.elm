module MLC.Editor.StateInList exposing (..)
{-| Describe me please...
-}

import MLC.Editor.Basics exposing (toDisplayString)
import MLC.Types exposing (..)
import SEd.Operations exposing (ScopeMeta, ScopeKind(..))

type alias StateInList =
    { elements: List Expression
    }


initialModel : StateInList
initialModel =
    { elements = []
    }

emptyMeta : ScopeMeta
emptyMeta = { displayName = "()", kind = NodeScope}


meta : StateInList -> ScopeMeta
meta s =
    { emptyMeta
        | displayName = toDisplayString (EList s.elements)
        }
--    case s.elements of
--        [] -> emptyMeta
--        EKey s :: _ -> { emptyMeta | displayName = displa"(:" ++ s ++ "..)" }
--        _ -> { emptyMeta | displayName = "(..)" }
