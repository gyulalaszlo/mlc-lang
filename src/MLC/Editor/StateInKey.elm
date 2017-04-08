module MLC.Editor.StateInKey exposing (..)
{-| Describe me please...
-}
import MLC.Editor.Basics exposing (toDisplayString)
import MLC.Types exposing (Expression(EKey))
import SEd.Operations exposing (ScopeKind(..), ScopeMeta, leafOperationIds)

type alias StateInKey = String


initialModel : StateInKey
initialModel = ""

emptyMeta : ScopeMeta
emptyMeta = { displayName = "()", kind = NodeScope, supports = leafOperationIds}


meta : StateInKey -> ScopeMeta
meta s =
    { emptyMeta
    | displayName = toDisplayString (EKey s)
    }
