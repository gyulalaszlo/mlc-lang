module Helpers.Css exposing (..)
{-| Describe me please...
-}

import Color
import Helpers.BSPSplitView
import Html exposing (Html)
import Html.Attributes

css : Html msg
css = Html.node "style"
        [Html.Attributes.type_ "text/css"]
        [Html.text cssBody ]


black = "#272822"
pink = "#f92672"
cyan = "#66d9ef"
green = "#a6e22e"
orange = "#fd971f"

pinkColor = Color.rgb 0xf9 0x26 0x72

cssBody : String
cssBody = """

.mkz-view { position:absolute; overflow:hidden; height:100%; width:100%; }
.mkz-view { font-family: Arial, Verdana, sans-serif; padding:0; margin:0; font-size: 13px; }
.mkz-view { background: #272822; color: #ddd;}


.mkz-view .cursor-scopes,
.mkz-view code,
.mkz-view pre,
.mkz-view button { font-family: "Fira Code", Monaco, Courier New; font-size: 12px; }




/* header */

.mkz-view button { outline: none; background: #222; color: #999; border: none; border-radius: 5px; margin: 0 0.5em; transition: background-color 0.3s, color 0.3s, border-bottom 0.2s; border-bottom: 3px solid }

.mkz-view button:hover { background: #f70; color: #000; border-bottom: 3px solid #c50; }
.mkz-view button.on { color: #f70; }
.mkz-view button.on:hover { color: #000; }

.mkz-view-header { z-index: 999; position: absolute; background: rgba(0,0,0,0.85); width:100%; height: 26px; padding: 3px }


/* Splits */

.mkz-view .split-pane { position:absolute; overflow-x:hidden; overflow-y:scroll; }

.mkz-view .code-view { position:absolute; top: 32px; left: 0; bottom: 0; right: 0; }



/* CURSOR-VIEW */


.mkz-view .cursor-view {   }
.mkz-view .cursor-scopes { background: #333;  }
.mkz-view .cursor-scope { display: inline-block; line-height: 1.4em; padding: 0.4em 1em;  border-radius: 2em; }

.mkz-view .cursor-scope-separator { border-right: 3px solid; border-radius: 2em; color: #555; }
.mkz-view .cursor-scope-separator:after { content: "->" }

.mkz-view .cursor-scope-nth,
.mkz-view .cursor-scope-leaf {  }

.mkz-view .cursor-scope .head {  color: #fd971f; }



.mkz-view .node-view {  }
.mkz-view .node-view.is-target { color: #fd971f; background: #532; }
.mkz-view .node-view.is-in-path { color: #c95; }

.mkz-view .leaf {  }
.mkz-view .leaf.is-target { color: #f70; background: #532; }
.mkz-view .leaf.is-in-path { color: #c95; }


.mkz-view .node-tree-view { }
.mkz-view .node-tree-view code { font-size: 15px; }


.node-tree-view { padding: 0 0 0 2em; }
.node-tree-view-inner {  padding: 0.3em, 1em; }
.node-tree-pre { white-space: pre-wrap; }






.mkz-view .token { cursor: pointer; border:none; transition: background-color 0.3s, color 0.3s; }
.mkz-view .token:hover { background: #444; border-radius: 2px; }
.mkz-view .token-keyword { color: #c33; }
.mkz-view .token-type { color: #39c; }
.mkz-view .token-symbol { color: #7c7; }
.mkz-view .token-comment { color: #666; }
.mkz-view .token-parenthesis,
.mkz-view .token-colon,
.mkz-view .token-semicolon { color: #444; }
.mkz-view .token-op,
.mkz-view .token-assign { color: #c90; }
.mkz-view .token-literal { color: #c09; }
.mkz-view .token-functionName { color: #5ca; }

.mkz-view .token-line-break { color: #3c3c3c; }
.mkz-view .token-line-break:before { content: "⏎"; }

.mkz-view .token-whiteSpace {}
.mkz-view .token-whiteSpace:hover {  background: #333;  }

.mkz-view .token-indent,
.mkz-view .token-outdent { color: #222; position:absolute; left: -1.5em; }

.mkz-view .token-outdent:before { content: ""; }
.mkz-view .token-indent:before { content: ""; }

.mkz-view .token-keyword:hover { background-color: #c33; color: #222; }
.mkz-view .token-type:hover { background-color: #39c; color: #222; }

.mkz-view .token-symbol:hover { background-color: #7c7; color: #222; }
.mkz-view .token-functionName:hover { background-color: #5ca; color: #222; }


.mkz-view .code-style-editor {}





/* Scrollbar styles */
::-webkit-scrollbar { width: 12px; height: 12px; }
::-webkit-scrollbar-track { background: none; border-radius: 10px; }
::-webkit-scrollbar-thumb { border-radius: 10px; background: rgba(255,255,255,0.1);  }
::-webkit-scrollbar-thumb:hover { background: rgba(0,0,0,0.1);  }



""" ++ Helpers.BSPSplitView.css { hoverBackground = pinkColor
                                , normalBackground = (Color.rgba 0 0 0 0.1) }
