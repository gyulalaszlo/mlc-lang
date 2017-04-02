module AstView.Css exposing (css)
{-| Describe me please...
|-}

import Html exposing (Html)
import Html.Attributes

css : Html msg
css = Html.node "style"
        [Html.Attributes.type_ "text/css"]
        [Html.text cssBody ]


cssBody : String
cssBody = """

.ast-view { background: #111110; color: #999;}

.ast-view,
.ast-view code,
.ast-view pre { font-family: "Fira Code", Monaco, Courier New; font-size: 12px; }

.ast-view code { padding: 0 0 0 2em; display:block }
.ast-view pre { line-heigth:1.2em; position:relative; }

.ast-view .c-code {  white-space: pre-wrap; background: #222220; padding: 1em; }

.ast-view .token { cursor: pointer; border:none; transition: background-color 0.3s, color 0.3s; }
.ast-view .token:hover { background: #444; border-radius: 2px; }
.ast-view .token-keyword { color: #c33; }
.ast-view .token-type { color: #39c; }
.ast-view .token-symbol { color: #7c7; }
.ast-view .token-comment { color: #666; }
.ast-view .token-parenthesis,
.ast-view .token-colon,
.ast-view .token-semicolon { color: #444; }
.ast-view .token-op,
.ast-view .token-assign { color: #c90; }
.ast-view .token-literal { color: #c09; }
.ast-view .token-functionName { color: #5ca; }

.ast-view .token-line-break { color: #3c3c3c; }
.ast-view .token-line-break:before { content: "⏎"; }

.ast-view .token-whiteSpace {}
.ast-view .token-whiteSpace:hover {  background: #333;  }

.ast-view .token-indent,
.ast-view .token-outdent { color: #322; position:absolute; margin-left: -2em; }

.ast-view .token-outdent:before { content: ""; }
.ast-view .token-indent:before { content: ""; }

.ast-view .token-keyword:hover { background-color: #c33; color: #222; }
.ast-view .token-type:hover { background-color: #39c; color: #222; }

.ast-view .token-symbol:hover { background-color: #7c7; color: #222; }
.ast-view .token-functionName:hover { background-color: #5ca; color: #222; }





"""