module MLC.Editor.StateInList exposing (..)
{-| Describe me please...
-}

import MLC.Types exposing (..)
import SEd.CursorView exposing (StackLevel)

type alias StateInList =
    { elements: List Expression
    }


initialModel : StateInList
initialModel =
    { elements = []
    }


meta : StateInList -> StackLevel
meta s =
    case s.elements of
        [] -> { name = "()" }
        EKey s :: _ -> { name = "(:" ++ s ++ "..)" }
        _ -> { name = "(..)" }
