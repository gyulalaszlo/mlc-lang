module SEd.Main.View exposing (..)
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import SEd.StateMachine exposing (StateMachine)
import SEd.CursorView as CursorView
import SEd.NodeTree as NodeTree
import SEd.UndoStack as UndoStack
import SEd.Model exposing (Msg(..), Model)
import Helpers.SplitLayout as SplitLayout
import Helpers.BSPSplitView as Bsp
import Helpers.CssBit as Css


type alias SM x s c n = StateMachine x (Msg s c n) (Model x s c n) -> Html (Msg s c n)

view : StateMachine x (Msg s c n) (Model x s c n) -> Html (Msg s c n)
view {state} =
    div [ class "StructureEditor-view mkz-view" ]
        [ Bsp.view <| mainView state
        , errorView state
        ]



nodeTreeView model = Bsp.leaf <|
    Html.div []
        [ Html.map CursorViewMsg <| CursorView.view model.cursorView
        , Html.map NodeTreeMsg <| NodeTree.view model.nodeTree
        ]


undoStackView model = Bsp.leaf <| Html.map UndoStackMsg <| UndoStack.view model.undoStack

mainView model = Bsp.horizontal (Bsp.FixedB <| Css.Pixel 250) (nodeTreeView model) (undoStackView model)

{-| subview for: node tree split
-}
nodeTreeSplit : Model x s c n -> Html (Msg s c n)
nodeTreeSplit model =
    div [ class "node-tree-split" ]
        [ Html.map NodeTreeMsg <| NodeTree.view model.nodeTree ]



{-| subview for: undo stack split
-}
undoStackSplit : Model x s c n -> Html (Msg s c n)
undoStackSplit model =
    div [ class "undo-stack-split" ]
        [ Html.map UndoStackMsg <| UndoStack.view model.undoStack
        ]



{-| shows errors
-}
errorView : Model x s c n -> Html (Msg s c n)
errorView model =
    div [ class "error-view" ]
        [ case model.error of
            Nothing -> text ""
            Just err ->
                div [class "error"]
                    [ Html.pre []
                        [ text <|  String.join "\n\n"  <|
                            model.traits.errorToStringList  err
                        ]
                    ]
        ]
