module SEd.Operations.Ops exposing (..)

type alias Id = Int

{-| Metadata about an operation
-}
type alias Meta =
  { name: String
  , id: Id
  , description: String
  , params: List String
  }



-- NAMESPACE: Editor
-- =========

type EditorOperation 
  = EditorStart 
  | EditorStop 
  | EditorCursor CursorOperation 
  | EditorNode NodeOperation 



type alias EditorOperationMeta =
  { start: Meta
  , stop: Meta
  }



defaultEditorMetas : EditorOperationMeta -> EditorOperation  -> Meta
defaultEditorMetas meta op =
  case op of
      EditorStart -> meta.start
      EditorStop -> meta.stop



defaultEditorMetas : EditorOperationMeta
defaultEditorMetas =
  {  start =
      { name = "Start"
      , id = 43520
      , params = []
      , description = """  """ }
  ,  stop =
      { name = "Stop"
      , id = 43520
      , params = []
      , description = """  """ }
      }


-- NAMESPACE: Cursor
-- =========

type CursorOperation cursor 
  = CursorSet (cursor) 
  | CursorMove MoveOperation 



type alias CursorOperationMeta =
  { set: Meta
  }



defaultCursorMetas : CursorOperationMeta -> CursorOperation cursor  -> Meta
defaultCursorMetas meta op =
  case op of
      CursorSet _ -> meta.set



defaultCursorMetas : CursorOperationMeta
defaultCursorMetas =
  {  set =
      { name = "Set"
      , id = 52224
      , params = ["cursor" ]
      , description = """ Sets the editing cursor to a new value """ }
      }


-- NAMESPACE: Move
-- =========

type MoveOperation 
  = MoveUp 
  | MoveDown 
  | MoveLeft 
  | MoveRight 



type alias MoveOperationMeta =
  { up: Meta
  , down: Meta
  , left: Meta
  , right: Meta
  }



defaultMoveMetas : MoveOperationMeta -> MoveOperation  -> Meta
defaultMoveMetas meta op =
  case op of
      MoveUp -> meta.up
      MoveDown -> meta.down
      MoveLeft -> meta.left
      MoveRight -> meta.right



defaultMoveMetas : MoveOperationMeta
defaultMoveMetas =
  {  up =
      { name = "Up"
      , id = 52240
      , params = []
      , description = """ Move the editing cursor up a logical level. """ }
  ,  down =
      { name = "Down"
      , id = 52241
      , params = []
      , description = """ Move the editing cursor down a logical level. """ }
  ,  left =
      { name = "Left"
      , id = 52240
      , params = []
      , description = """ Move the editing cursor left at the logical level. """ }
  ,  right =
      { name = "Right"
      , id = 52241
      , params = []
      , description = """ Move the editing cursor right at the logical level. """ }
      }


-- NAMESPACE: Node
-- =========

type NodeOperation cursor node 
  = NodeInsert (node) 
  | NodeDelete 
  | NodeReplace (node) 



type alias NodeOperationMeta =
  { insert: Meta
  , delete: Meta
  , replace: Meta
  }



defaultNodeMetas : NodeOperationMeta -> NodeOperation cursor node  -> Meta
defaultNodeMetas meta op =
  case op of
      NodeInsert _ -> meta.insert
      NodeDelete -> meta.delete
      NodeReplace _ -> meta.replace



defaultNodeMetas : NodeOperationMeta
defaultNodeMetas =
  {  insert =
      { name = "Insert"
      , id = 65281
      , params = ["node" ]
      , description = """ Inserts a new node at the current editing cursor. """ }
  ,  delete =
      { name = "Delete"
      , id = 65282
      , params = []
      , description = """ Deletes the currently selected node. """ }
  ,  replace =
      { name = "Replace"
      , id = 65282
      , params = ["node" ]
      , description = """ Replaces the currently selected node with the given new node. """ }
      }

