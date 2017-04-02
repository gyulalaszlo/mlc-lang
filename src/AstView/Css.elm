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


.ast-view { font-family: Arial, Verdana, sans-serif; }
.ast-view { background: #111110; color: #999;}

.ast-view code,
.ast-view pre { font-family: "Fira Code", Monaco, Courier New; font-size: 12px; }

.ast-view code { padding: 0 0 0 2em; display:block }
.ast-view pre { line-heigth:1.2em; position:relative; }

.ast-view .c-code {  white-space: pre-wrap; background: #222220; padding: 1em; margin:0; }

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
.ast-view .token-outdent { color: #222; position:absolute; left: -1.5em; }

.ast-view .token-outdent:before { content: ""; }
.ast-view .token-indent:before { content: ""; }

.ast-view .token-keyword:hover { background-color: #c33; color: #222; }
.ast-view .token-type:hover { background-color: #39c; color: #222; }

.ast-view .token-symbol:hover { background-color: #7c7; color: #222; }
.ast-view .token-functionName:hover { background-color: #5ca; color: #222; }

.ast-view .table { }
.ast-view .table select { font-family: "Fira Code", Monaco, Courier New; border: none; background:none; color: #ccc; min-width:3em; }


.ast-view { position:absolute; overflow:hidden; height:100%; width:100%; }
.ast-view .code-view {  }
.ast-view .code-style-editor { background: #111; position: absolute; right: 0; top:0;   height: 100%; overflow-y: scroll; padding: 0 1em; box-shadow: -5px 0 10px rgba(0,0,0,0.5); }

.ast-view .code-style-editor th {font-size:9px;}
.ast-view .code-style-editor td.ws-key {font-size:13px;}



.ast-view button { outline: none; }


"""