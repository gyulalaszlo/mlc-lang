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

view : StateMachine x (Msg s c n) (Model x s c n) -> Html (Msg s c n)
view {state} =
    div [ class "StructureEditor-view mkz-view" ]
        [ Html.map CursorViewMsg <| CursorView.view state.cursorView
        , SplitLayout.view
                state.splitLayout
                [ Html.map NodeTreeMsg <| NodeTree.view state.nodeTree
                , Html.map UndoStackMsg <| UndoStack.view state.undoStack
                ]

        , case state.error of
            Nothing -> text ""
            Just err ->
                div [class "error"]
                    [ Html.pre []
                        [ text <|  String.join "\n\n"  <|
                            state.traits.errorToStringList  err
                        ]
                    ]
        ]
