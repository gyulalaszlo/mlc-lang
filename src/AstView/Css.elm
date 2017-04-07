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


.ast-view { font-family: Arial, Verdana, sans-serif; padding:0; margin:0; font-size: 13px; }
.ast-view { background: #222; color: #999;}


.ast-view code,
.ast-view pre,
.ast-view button { font-family: "Fira Code", Monaco, Courier New; font-size: 12px; }

.ast-view code { padding: 0 0 0 2em; display:block; background: #111; }
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


.ast-view { position:absolute; overflow:hidden; height:100%; width:100%; }

.ast-view .code-view { position:absolute; top: 32px; left: 0; bottom: 0; right: 0; }
.ast-view .code-style-editor {}

/* code-style editor */

.ast-view .table { line-height:1.4em; width:100%; }
.ast-view .table select { font-family: "Fira Code", Monaco, Courier New; border: none; background:none; color: #ccc; min-width:3em; }

.ast-view .code-style-editor .code-style-header { padding: 0.2em 0;  }
.ast-view .code-style-editor .code-style-header input { width: 80%; }


.ast-view .code-style-editor th {font-size:9px;}
.ast-view .code-style-editor td.ws-key {font-size:13px; border-bottom: 1px solid #222; }


/* Changed - unchanged rules */

.ast-view .code-style-editor tr.rule-changed td,
.ast-view .code-style-editor tr.rule-changed td select { color: #ccc; }

.ast-view .code-style-editor tr.rule-unchanged td,
.ast-view .code-style-editor tr.rule-unchanged td select { color: #555; }



/* header */

.ast-view button { outline: none; background: #222; color: #999; border: none; border-radius: 5px; margin: 0 0.5em; transition: background-color 0.3s, color 0.3s, border-bottom 0.2s; border-bottom: 3px solid }

.ast-view button:hover { background: #f70; color: #000; border-bottom: 3px solid #c50; }
.ast-view button.on { color: #f70; }
.ast-view button.on:hover { color: #000; }

.ast-view-header { z-index: 999; position: absolute; background: rgba(0,0,0,0.85); width:100%; height: 26px; padding: 3px }


/* Scrollbar styles */
::-webkit-scrollbar { width: 12px; height: 12px; }

::-webkit-scrollbar-track { background: none; border-radius: 10px; }

::-webkit-scrollbar-thumb { border-radius: 10px; background: rgba(255,255,255,0.1);  }

::-webkit-scrollbar-thumb:hover { background: rgba(0,0,0,0.1);  }



.ast-view .split-pane { position:absolute; overflow-x:hidden; overflow-y:scroll; }

"""