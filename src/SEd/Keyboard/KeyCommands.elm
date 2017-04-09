module SEd.Keyboard.KeyCommands exposing (..)
{-| Describe me please...
-}

import Dict exposing (Dict)
import Keyboard.Keys exposing (Key, KeyEvent)

type alias Predicate = (KeyEvent -> Bool)
type alias Applier msg = (KeyEvent -> Cmd msg)

type alias KeyCommand msg =
    { description: String
    , predicate: Predicate
    , apply:  Applier msg
    }



keyCommand : Predicate -> Applier msg -> String -> KeyCommand msg
keyCommand pred fn desc =
    { description = desc, predicate = pred, apply = fn }



{-| A command is a triggerable operation
-}
type alias Command scope msg = msg -> scope -> (scope, Cmd msg)

{-| Describes a scope from the command perspective
-}
type alias ScopeCommandTraits msg scope =
    { commands: ( scope -> List (Command msg scope) )
    }


--type alias ScopeId = String
--type alias ScopeList scope = Dict ScopeId ScopeKind scope
--
--
--getScopeKind : (scope -> ScopeId) -> ScopeList -> Maybe ScopeKind
--getScopeKind s = Nothing
--
--
--
--
--type alias ChildKinds scope = scope ->  List ScopeId



{-| Gets the scope metadata
-}
type alias GetScopeKind scope =
    (scope -> ScopeKind scope)




{-| The different kind of command scopes
-}
type ScopeKind scope
    = LeafScope (LeafMeta scope)
    | RecordScope (RecordMeta scope)
    | ListScope (ListMeta scope)


scopeKindToString : ScopeKind scope -> String
scopeKindToString s =
    case s of
        LeafScope _ -> "Leaf"
        RecordScope _ -> "Record"
        ListScope _ -> "List"


-- SCOPE METADATA


{-| Traits for leaf scopes
-}
type alias LeafMeta scope =
    { meta: scope -> FieldMeta scope
    , toString: scope -> String
    }


{-| Records are scopes with fixed fields
-}
type alias RecordMeta scope =
    { meta: (scope -> List (FieldMeta scope))
    }



{-| Lists are scopes with a dynamic number of elements of the same type (for now)
Specialization should be handled by scope specialization
-}
type alias ListMeta scope =
    { metaFor: scope -> FieldMeta scope
    , children: scope -> List scope
    }







-- FIELD METADATA


{-| Value type of a field
-}
type FieldKind scope
    = StringField
    | IntegerField

    | ChildField (ChildFieldMeta scope)


{-| Describes the possible children for a field
-}
type alias ChildFieldMeta scope =
    { name: String
    , scopes: List (ChildFieldKind scope)
    }


{-| A possible child field type
-}
type alias ChildFieldKind scope =
    { name: String
    , scope: ScopeKind scope
    }


{-| Metadata about a field in records and lists
-}
type alias FieldMeta scope =
    { name: String
    , kind: FieldKind scope
    }









