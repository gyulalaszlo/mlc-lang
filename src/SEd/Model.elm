module SEd.Model exposing (Model, Traits, fromTraits, Msg(..))
{-| Describe me please...
-}

import Helpers.SplitLayout as SplitLayout
import Keyboard exposing (KeyCode)
import SEd.CursorView as CursorView exposing (StackLevel)
import SEd.UndoStack as UndoStack
import SEd.NodeTree as NodeTree
import SEd.NodeView as NodeView

type Msg state cursor node
    = KeyPress KeyCode
    | KeyDown KeyCode
    | KeyUp KeyCode

    | NodeTreeMsg NodeTree.Msg
    | CursorViewMsg (CursorView.Msg state cursor)
    | UndoStackMsg (UndoStack.Msg cursor node)
    | SplitLayoutMsg SplitLayout.Msg


    | NoOp



type alias Model error state cursor node =
    { current: state
    , stack: List state
    , data: node
    , lastKeys: List Char
    , cursor: cursor

    , error: Maybe error

    -- Sub component: NodeTree
    , nodeTree: NodeTree.Model

    -- Sub component: CursorView
    , cursorView: CursorView.Model state cursor

    -- Sub component: UndoStack
    , undoStack: UndoStack.Model cursor node

    -- Sub component: SplitLayout
    , splitLayout: SplitLayout.Model

    , traits: Traits state cursor node
    }



type alias Traits state cursor node =
    { initialData: node
    , initialCursor: cursor
    , initialState: state

    , toNodeTreeMeta: (cursor -> node -> NodeView.Model)

    , cursorToStringList: (cursor -> List String)
    , stateToString: (state -> String)
    , nodeToString: (node -> String)

    , stateMeta: state -> StackLevel

    }


cursorTraits : Traits s c n -> CursorView.Traits s c
cursorTraits { cursorToStringList, stateToString, stateMeta, initialState } =
    { cursorToStringList = cursorToStringList
    , stateToString = stateToString
    , stateMeta = stateMeta

    , initialState = initialState
    }


undoStackTraits : Traits s c n -> UndoStack.Traits c n
undoStackTraits { cursorToStringList, nodeToString } =
    { cursorToStringList = cursorToStringList
    , nodeToString = nodeToString
    }


fromTraits : Traits s c n -> Model x s c n
fromTraits traits =
    { current = traits.initialState
    , stack = []
    , data = traits.initialData
    , lastKeys = []
    , cursor = traits.initialCursor

    , error = Nothing
    , nodeTree = NodeTree.initialModel
--    , cursorView = CursorView.initialModel traits
    , cursorView = CursorView.initialModel <| cursorTraits traits
    , undoStack =  UndoStack.modelFromTraits <| undoStackTraits traits
    , splitLayout = SplitLayout.initialModel


    , traits = traits


    }
