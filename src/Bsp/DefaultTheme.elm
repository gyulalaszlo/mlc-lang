module Bsp.DefaultTheme
    exposing
        ( css
        , toolbarTraits
        )

{-| Describe me please...
-}

import Bsp.Cursor exposing (Cursor(..), parentCursor)
import Bsp.Model exposing (Model)
import Bsp.Msg exposing (..)
import Bsp.Ratio exposing (Ratio(..))
import Bsp.Traits exposing (..)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), RotateDirection(..), SplitMeta, SplitModel(Node), nodeToString)
import Colors.Monokai
import Css
import Error
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed


toolbarTraits : (l -> String) -> List l -> ToolbarTraits m l s
toolbarTraits labelFn emptySelectable =
    { normal = normalModeTraitsFor labelFn emptySelectable
    , layoutEditing = editingModeTraitsFor labelFn emptySelectable
    }



-- REGULAR TOOLBARS ------------------------------------------------------------

--
--splitToolbar : (Cursor -> Cursor) -> SplitMeta Id -> s -> Html (Msg m l)
--splitToolbar _ _ _ =
--    Html.text ""
--

btn : Msg m l -> String -> Html (Msg m l)
btn click label =
    Html.button [ onClick click ] [ text label ]



-- LAYOUT EDITING VIEW ---------------------------------------------------------


leafLabel : (l -> String) -> Cursor -> Id -> l -> Html (Msg m l)
leafLabel labelFn c id l =
    div
        [ class "layout-editing-leaf-label"
        , onClick (Select c)
        ]
        [ span [ class "label" ] [ text <| labelFn l ]
        , span [ class "id" ] [ text <| toString id ]
        ]


layoutEditingLeafView : (l -> String) -> Cursor -> Id -> l -> s -> Html (Msg m l)
layoutEditingLeafView labelFn c id l _ =
    let
        selBtn cc label =
            btn (Select cc) label
    in
        Html.div []
            [ leafLabel labelFn c id l
            , btn (RotateParent CW c) <| "<~"
            , btn (RotateParent CCW c) <| "~>"
            ]


layoutSelectedEditingLeafView : (l -> String) -> Cursor -> Id -> l -> s -> Html (Msg m l)
layoutSelectedEditingLeafView labelFn c id l _ =
    let
        selBtn cc label =
            btn (Select cc) label
    in
        Html.div []
            [ leafLabel labelFn c id l
            , btn (SplitAt c Horizontal l) "||"
            , btn (SplitAt c Vertical l) "--"
            , btn (DeleteAt c) "DEL"
            ]


--layoutEditingSplitView : (Cursor -> Cursor) -> SplitMeta Id -> s -> Html (Msg m l)
--layoutEditingSplitView cursorFn { a, b, direction, ratio } _ =
--    let
--        cursor =
--            cursorFn CHead
--
--        dirBtn dir label =
--            btn (SetDirection cursor dir) label
--
--        canRotate =
--            case ( a, b ) of
--                ( _, Node _ ) ->
--                    True
--
--                ( Node _, _ ) ->
--                    True
--
--                _ ->
--                    False
--    in
--        div [ class "layout-editing-split-view" ]
--            [ btn (Select cursor) <| "."
--            , text <| " A=" ++ nodeToString a
--            , text <| " B=" ++ nodeToString b
--            , btn (Rotate CW cursor) <| "<-R"
--            , btn (Rotate CCW cursor) <| "R->"
--            , btn (SwapLR cursor) <| "AB -> BA"
--            , case direction of
--                Vertical ->
--                    dirBtn Horizontal "-- H --"
--
--                Horizontal ->
--                    dirBtn Vertical "|| V ||"
--            ]


--globalLayoutEditor : Cursor -> s -> Html (Msg m l)
--globalLayoutEditor c s =
--    div [ class "layout-editor-global-header" ]
--        [ btn (SetLayoutEditingMode NotEditingLayout) "Done editing"
--        , text <| toString c
--        ]



-- TRAITS ======================================================================


normalModeTraitsFor :(l -> String) -> List l -> NodeViewModeTraits m l s
normalModeTraitsFor labelFn empties =
    { normal = normalTraits labelFn empties
    , selected = selectedTraits labelFn empties
    , global = normalGlobal
    }


editingModeTraitsFor :(l -> String) -> List l -> NodeViewModeTraits m l s
editingModeTraitsFor labelFn empties =
    { normal = layoutEditingNormalTraits labelFn empties
    , selected = layoutEditingSelectedTraits labelFn empties
    , global = editingGlobal
    }


normalTraits : (l -> String) -> List l -> NodeViewBaseTraits m l s
normalTraits labelFn empties =
    { leaf = normalLeaf
    , split = normalSplit
    , empty = normalEmpty labelFn empties
    }


selectedTraits : (l -> String) -> List l -> NodeViewBaseTraits m l s
selectedTraits labelFn empties =
    { leaf = selectedLeaf
    , split = selectedSplit
    , empty = selectedEmpty labelFn empties
    }

layoutEditingNormalTraits : (l -> String) -> List l -> NodeViewBaseTraits m l s
layoutEditingNormalTraits labelFn empties =
    { leaf = layoutEditingNormalLeaf
    , split = layoutEditingNormalSplit
    , empty = normalEmpty labelFn empties
    }


layoutEditingSelectedTraits : (l -> String) -> List l -> NodeViewBaseTraits m l s
layoutEditingSelectedTraits labelFn empties =
    { leaf = layoutEditingSelectedLeaf
    , split = layoutEditingSelectedSplit
    , empty = selectedEmpty labelFn empties
    }


-- NORMAL ======================================================================

globalToolbar mode shared model =
    div [ class "global-toolbar" ]
        [ case mode of
            NotEditingLayout -> btn (SetLayoutEditingMode EditingLayoutBlocks) "Edit layout"
            EditingLayoutBlocks -> btn (SetLayoutEditingMode NotEditingLayout) "Done"
        , text " | "
        , globalToolbarCurrent model
        ]


globalToolbarCurrent model =
    case model of
--        Err err -> span [ class "error" ] [ text <| String.fromList <| List.take 30 <| String.toList <| Error.errorToString err ]
        Err err -> span [ class "error" ] [ text <| Error.errorToString err ]
        Ok {cursor, id} -> span [ class "current" ] [ text <| toString cursor ]

normalGlobal shared model = globalToolbar NotEditingLayout shared model
editingGlobal shared model = globalToolbar EditingLayoutBlocks shared model



-- NOT SELECTED


normalLeaf : LeafViewFn m l s
normalLeaf view model =
    div [ class "normal-leaf" ]
        [ view model
        , div [ class "leaf-id" ]
            [ text <| toString model.id
            ]
        ]


normalSplit : SplitViewFn m l s
normalSplit { shared, meta, cursor } html =
    html


normalEmpty : (l -> String) -> List l -> Cursor -> s -> Html (Msg m l)
normalEmpty labelFn empties cursor shared =
    Html.div
        []
        [ Html.Keyed.ul [] <|
            List.indexedMap (\i l-> (toString i, l)) <|
            List.map (\l -> Html.li [] [ btn (SplitAt cursor Horizontal l) <| "to: " ++ labelFn l] ) empties

        ]



-- SELECTED


selectedLeaf : LeafViewFn m l s
selectedLeaf view model =
    view model


selectedSplit : SplitViewFn m l s
selectedSplit { shared, meta, cursor } html =
    html


selectedEmpty : (l -> String) -> List l -> Cursor -> s -> Html (Msg m l)
selectedEmpty =
    normalEmpty



-- LAYOUT EDITING ==============================================================


editingTraits : NodeViewBaseTraits m l s
editingTraits =
    { leaf = layoutEditingNormalLeaf
    , split = layoutEditingNormalSplit
    , empty = layoutEditingNormalEmpty
    }


layoutEditingNormalLeaf : LeafViewFn m l s
layoutEditingNormalLeaf view model =
    view model


layoutEditingNormalSplit : SplitViewFn m l s
layoutEditingNormalSplit { shared, meta, cursor } html =
    div [ class "layout-editing-split-wrapper  layout-editing-split-wrapper-not-selected" ]
        [ layoutEditingNormalSplitView cursor meta shared
        , div [class "layout-editing-inner-wrap" ] [ html ]
        ]


layoutEditingNormalEmpty : EmptyViewFn m l s
layoutEditingNormalEmpty cursor shared =
    text "Empty"


layoutEditingNormalSplitView : (Cursor -> Cursor) -> SplitMeta Id -> s -> Html (Msg m l)
layoutEditingNormalSplitView cursorFn { a, b, direction, ratio } _ =
        div [ class "layout-editing-split-view layout-editing-split-view-not-selected", onClick (Select (cursorFn <| CHead )) ]
--            [ btn (Select (cursorFn <| CHead)) "."
            [ text <| " A=" ++ nodeToString a
            , text <| " B=" ++ nodeToString b
            ]

-- SELECTED


layoutEditingSelectedLeaf : LeafViewFn m l s
layoutEditingSelectedLeaf view model =
    view model


layoutEditingSelectedSplit : SplitViewFn m l s
layoutEditingSelectedSplit { shared, meta, cursor } html =
    div [ class "layout-editing-split-wrapper layout-editing-split-wrapper-selected" ]
        [ layoutEditingSplitView cursor meta shared
        , div [class "layout-editing-inner-wrap" ] [html ]]


layoutEditingSelectedEmpty : EmptyViewFn m l s
layoutEditingSelectedEmpty cursor shared =
    text "Empty"


layoutEditingSplitView : (Cursor -> Cursor) -> SplitMeta Id -> s -> Html (Msg m l)
layoutEditingSplitView cursorFn { a, b, direction, ratio } _ =
    let
        cursor =
            cursorFn CHead

        dirBtn dir label =
            btn (SetDirection cursor dir) label

        resizeBtn r label = btn (ResizeAt r cursor) label

        canRotate =
            case ( a, b ) of
                ( _, Node _ ) ->
                    True

                ( Node _, _ ) ->
                    True

                _ ->
                    False
    in
        div [ class "layout-editing-split-view layout-editing-split-selected" ]
            [ text <| " A=" ++ nodeToString a
            , text <| " B=" ++ nodeToString b
            , btn (Rotate CW cursor) <| "<-R"
            , btn (Rotate CCW cursor) <| "R->"
            , btn (SwapLR cursor) <| "AB -> BA"
            , case direction of
                Vertical ->
                    dirBtn Horizontal "-- H --"

                Horizontal ->
                    dirBtn Vertical "|| V ||"
            , resizeBtn (FixedB 66) "1+2"
            , resizeBtn (Equal) "=="
            , resizeBtn (FixedA 66) "1+2"
            ]

-- CSS -------------------------------------------------------------------------


{-| CSS parts for childView
-}
css : String
css =
    let themeColor cfn = (Css.color << cfn) Colors.Monokai.theme
    in Css.css
        [ ( "leaf-background", themeColor .background )
        , ( "leaf-text", themeColor .text )
        , ( "leaf-selection", themeColor .selection )
        , ( "split-background", "black" )
        , ( "leaf-radius", "5px" )
        , ( "leaf-margin", "0.1em" )

        , ( "color-darkbg", Css.color <| Colors.Monokai.darkBlack)

        -- layout-editing
        , ( "layout-border-size", "0.2em" )
        , ( "layout-margin", "0.3em" )
        , ( "layout-editing-margin", "0.3em" )

        , ( "layout-editing-split-border-style", "0.1em dotted" )
        -- toolbar data
        , ( "toolbar-margin", "0.3em" )
        , ( "toolbar-height", "1.6em" )
        ]
        """
.child-view {  }

.btn { display:inline-block; padding: 0.3em 1em; cursor: pointer; font-size:0.8em;}
.btn:hover { text-decoration:underline; }

.bsp-root-view { background: {{ split-background }}; color: white; position: absolute; left: 0; right:0; top:0; bottom:0; }

/* LEAF BASICS --------------------------- */

.bsp-view-split-wrapper-leaf { margin: {{ leaf-margin }}; border-top: {{ layout-border-size }} solid transparent;}
.bsp-view-split-wrapper-leaf.bsp-view-split-wrapper-not-selected { background-color: {{ color-darkbg }}; color: #ccc; }
.bsp-view-split-wrapper-leaf.bsp-view-split-wrapper-selected { background-color: {{ leaf-background }}; color: {{ leaf-text }}; border-top-color: {{ leaf-selection }}; }

/* NODE WRAPPER -------------------------- */

.bsp-view-split-wrapper { border-radius: {{ leaf-radius }}; }

.layout-editor-global-header,
.bsp-view-node-split-horizontal-toolbar,
.bsp-view-node-split-vertical-toolbar { position:absolute; top:0; left: 0; right: 0; z-index:999; }


.bsp-view-split-wrapper {}


/* LAYOUT EDITING ===================================== */

.bsp-view-split-wrapper-editing { margin: {{ layout-editing-margin }}; }
.bsp-view-split-wrapper-editing.bsp-view-split-wrapper-leaf { background: {{ leaf-background }}; }
.bsp-view-split-wrapper-editing.bsp-view-split-wrapper-node {}

.bsp-view-split-wrapper-editing.bsp-view-split-wrapper-selected { border-color: {{ leaf-selection }}; }


.a--bsp-view-split-wrapper-editing .bsp-view-node-split-horizontal,
.a--bsp-view-split-wrapper-editing .bsp-view-node-split-vertical { margin-top: {{ toolbar-height }}; }

.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-vertical-toolbar,
.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-horizontal-toolbar { margin: {{ toolbar-margin }}; }



.bsp-view-split-wrapper-root { margin-top: {{ toolbar-height }}; }


/*
.node.node-split.node-vertical.node-b { border-top: {{ layout-editing-split-border-style }} black; }
.node.node-split.node-horizontal.node-b { border-left: {{ layout-editing-split-border-style }} black; }
*/



.layout-editing-leaf-label { position: absolute; font-size: 3em; color: #888; bottom: 0; left: 0; right: 0; text-align: center; cursor: pointer; }
.layout-editing-leaf-label:hover { color: {{ leaf-selection }}; }


/* -- Split editor ----------------------------- */

.layout-editing-split-wrapper {  border: {{ layout-border-size }} solid {{ split-background }}; background: {{ leaf-background }}; border-radius: {{ leaf-radius }}; margin: {{ layout-editing-margin }};  background-color: {{ color-darkbg }};  }
.layout-editing-split-wrapper-selected { border-color: {{ leaf-selection }}; }

.layout-editing-split-wrapper { position: absolute; top: 0; left: 0; right:0; bottom: 0; overflow: hidden; }

.bsp-view-split-wrapper-selected.bsp-view-split-wrapper-editing > .node.node-split.node-b { border-color: {{ leaf-selection }}; }

.layout-editing-split-view.layout-editing-split-selected { background: {{ leaf-selection }}; }
.layout-editing-split-view { background: {{ split-background }};  cursor: pointer; }
.layout-editing-split-view-not-selected { cursor: pointer; }
.layout-editing-split-view-not-selected:hover { color: {{ leaf-selection }};}

/* -- SPLIT HEADER -- */

.layout-editing-split-view { position: absolute; top: 0; height: {{ toolbar-height }}; left: 0; right:0;  overflow: hidden; text-align:center; }

/* -- WRAP -- */

.layout-editing-inner-wrap { position: absolute; top: {{ toolbar-height }}; left: 0; right:0; bottom: 0; }


.leaf-id { position: absolute; right: 0; top: 0; background: {{ leaf-text }}; color: {{ leaf-background }};  padding: 0.2em 1em; }
"""
