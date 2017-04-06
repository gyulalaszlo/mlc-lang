module MLC.Editor.State exposing (State(..), inList, inKey)
{-| Describe me please...
-}

import MLC.Editor.StateInKey exposing (StateInKey)
import MLC.Editor.StateInList exposing (StateInList)

type State
    = InList StateInList
    | InKey StateInKey



inList : State
inList = InList MLC.Editor.StateInList.initialModel

inKey : State
inKey = InKey MLC.Editor.StateInKey.initialModel
