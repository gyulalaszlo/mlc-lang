module SEd.Operations exposing (..)
{-| Defines possible operations
-}

import Keyboard exposing (KeyCode)


type Mode
    = Normal
    | Insert



type Operation cursor node
    = InsertNodeAt cursor node
    | ReplaceNodeAt cursor node
    | DeleteNodeAt cursor


-- OPERATION IDS

insertNodeAtId = 0xff01
replaceNodeAtId = 0xff02
deleteNodeAtId = 0xff03


listOperationIds : List OperationId
listOperationIds = [ insertNodeAtId, replaceNodeAtId, deleteNodeAtId ]

leafOperationIds : List OperationId
leafOperationIds = [ ]

--
--idOf : Operation cursor node -> OperationMeta
--idOf op =
--    case op of
--        InsertNodeAt _ _ -> insertNodeAtId
--        ReplaceNodeAt _ _ -> replaceNodeAtId
--        DeleteNodeAt _ -> deleteNodeAtId



type alias OperationId = Int

type alias OperationMeta =
  { name: String
  , id: Int
  , description: String
  , params: List String
  }




type ScopeKind
    = LeafScope
    -- A scope with any children (so they support
    | NodeScope


type alias ScopeMeta =
    { displayName: String
    , kind: ScopeKind
    , supports: List OperationId
    }


type alias StringConverter v =
    { from: (String -> Maybe v)
    , to: (v -> String)
    }


type alias ScopeTraits v =
    { meta: (v -> ScopeMeta)

--    , str: StringConverter
    }






