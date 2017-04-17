module Bsp.DefaultTheme
    exposing
        ( css
        , toolbarTraits
        )

{-| Describe me please...
-}

import Bsp.Cursor exposing (Cursor(..), parentCursor)
import Bsp.RootModel exposing (Id, LayoutEditingMode(..), Model, Msg(..), ToolbarTraits)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), RotateDirection(..), SplitMeta, SplitModel(Node))
import Colors.Monokai
import Css
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


toolbarTraits : (l -> String) -> ToolbarTraits m l s
toolbarTraits labelFn =
    { split = splitToolbar
    , leafLayoutEditing = layoutEditingLeafView labelFn
    , leafSelectedLayoutEditing = layoutSelectedEditingLeafView labelFn
    , splitLayoutEditing = layoutEditingSplitView
    , globalLayoutEditor = globalLayoutEditor

    }


splitToolbar : Cursor -> s ->  Html (Msg m l)
splitToolbar cursor _ = Html.text ""

btn : Msg m l -> String -> Html (Msg m l)
btn click label  = Html.button [ onClick click ] [ text label ]


-- LAYOUT EDITING VIEW ---------------------------------------------------------

leafLabel : (l -> String) -> Cursor -> Id -> l ->  Html (Msg m l)
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
    let selBtn cc label= btn (Select cc) label
    in
         Html.div []
            [ leafLabel labelFn c id l
--            , selBtn c "SELECT"
            ]

layoutSelectedEditingLeafView : (l -> String) -> Cursor -> Id -> l -> s -> Html (Msg m l)
layoutSelectedEditingLeafView labelFn c id l _ =
    let selBtn cc label= btn (Select cc) label
    in
         Html.div []
            [ leafLabel labelFn c id l
--            , selBtn c "SELECT"
            , btn (SplitAt c Horizontal l) "||"
            , btn (SplitAt c Vertical l) "--"
--            , parentCursor c
--                |> Maybe.map (\cc -> selBtn cc "PARENT")
--                |> Maybe.withDefault (text "")
            ]


layoutEditingSplitView : (Cursor -> Cursor) -> SplitMeta Id -> Html (Msg m l)
layoutEditingSplitView cursorFn {a,b,direction,ratio} =
    let cursor = cursorFn CHead
        dirBtn dir label= btn (SetDirection cursor dir) label
        canRotate = case (a,b) of
            (_, Node _) -> True
            (Node _, _) -> True
            _ -> False

    in div [ class "layout-editing-split-view" ]
        [ btn (Select cursor) <| "."

        , if canRotate
            then span []
                    [ btn (Rotate cursor CW) <| "<-R"
                    , btn (Rotate cursor CCW) <| "R->"
                    ]
            else text ""

        , btn (SwapLR cursor) <| "AB -> BA"
        , case direction of
            Vertical -> dirBtn Horizontal "-- H --"
            Horizontal -> dirBtn Vertical "|| V ||"
        ]

globalLayoutEditor : Cursor -> s -> Html (Msg m l)
globalLayoutEditor c s =
    div [ class "layout-editor-global-header" ]
        [ btn (SetLayoutEditingMode NotEditingLayout) "Done editing"
        , text <| toString c
        ]

-- CSS -------------------------------------------------------------------------

{-| CSS parts for childView
-}
css : String
css =
    Css.css
        [ ( "leaf-background", Css.color (Colors.Monokai.black) )
        , ( "leaf-text", Css.color (Colors.Monokai.white) )
        , ( "leaf-selection", Css.color (Colors.Monokai.orange) )
        ]
        """
.child-view {  }

.btn { display:inline-block; padding: 0.3em 1em; cursor: pointer; font-size:0.8em;}
.btn:hover { text-decoration:underline; }

.bsp-root-view { background: black; position: absolute; left: 0; right:0; top:0; bottom:0; }

.bsp-view-split-wrapper-leaf { background-color: #111213; color: #ccc; margin:2px;  }
.bsp-view-split-wrapper-leaf-selected { background-color: {{ leaf-background }}; color: {{ leaf-text }}; }

.bsp-view-node {}
.bsp-view-node-split {}
.bsp-view-node-split-horizontal {}
.bsp-view-node-split-horizontal-a {}
.bsp-view-node-split-horizontal-b {}
.bsp-view-node-split-horizontal-toolbar { }

.bsp-view-node-split-vertical {}
.bsp-view-node-split-vertical-a {}
.bsp-view-node-split-vertical-b {}
.bsp-view-node-split-vertical-toolbar { }


.bsp-view-split-wrapper { border-radius: 1em; }
.bsp-view-split-wrapper-node {}
.bsp-view-split-wrapper-selected-node {}
.bsp-view-split-wrapper-selected-node-horizontal {}
.bsp-view-split-wrapper-selected-node-vertical {}
.bsp-view-split-wrapper-node-horizontal {}
.bsp-view-split-wrapper-node-vertical {}

.bsp-view-split-wrapper-node-vertical-selected,
.bsp-view-split-wrapper-node-horizontal-selected { border: 0.2em solid {{ leaf-selection }}; }


.layout-editor-global-header,
.bsp-view-node-split-horizontal-toolbar,
.bsp-view-node-split-vertical-toolbar { position:absolute; top:0; left: 0; right: 0; z-index:999; }


.bsp-view-split-wrapper {}

/* LAYOUT EDITING ===================================== */

.bsp-view-split-wrapper-editing-layout { margin: 0.3em; }
.bsp-view-split-wrapper-editing-layout-leaf { background: {{ leaf-background }}; border:0.2em dotted black; }
.bsp-view-split-wrapper-editing-layout-node { border: 0.2em solid black; background: {{ leaf-background }}; }
.bsp-view-split-wrapper-editing-layout-node-vertical {}
.bsp-view-split-wrapper-editing-layout-node-horizontal {}

.bsp-view-split-wrapper-editing-layout-node-selected {}
.bsp-view-split-wrapper-editing-layout-leaf-selected { color: {{ leaf-selection }}; border: 0.2em solid;  }

.bsp-view-split-wrapper-editing-layout-node-selected,
.bsp-view-split-wrapper-editing-layout-leaf-selected { border-color: {{ leaf-selection }}; }

.bsp-view-split-wrapper-editing-layout-node-horizontal-selected {}
.bsp-view-split-wrapper-editing-layout-node-vertical-selected {}

.bsp-view-split-wrapper-editing-layout-node-horizontal-selected,
.bsp-view-split-wrapper-editing-layout-node-vertical-selected { background: {{ leaf-selection }}; border-color: {{ leaf-selection }}; }

.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-horizontal,
.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-vertical-a { margin-top: 32px; }

.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-vertical-toolbar,
.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-horizontal-toolbar { margin-top: 0.3em; }



.bsp-view-split-wrapper-root-edited { margin-top:32px; }





.layout-editing-leaf-label { position: absolute; font-size: 3em; color: #888; bottom: 0; left: 0; right: 0; text-align: center; cursor: pointer; }
.layout-editing-leaf-label:hover { color: {{ leaf-selection }}; }
"""
