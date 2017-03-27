module Bootstrap exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text, program)
import Html.Attributes exposing (style, type_)
import SSA.SSAForm
import SSA.Main as SSAView
import SSA.SSASample exposing (sample)
import Task


type alias Model =
    { code: String
    , ssa: SSAView.Model
    }

initialModel = { code = "", ssa = SSAView.initialModel }

type Msg
    = Init
    | SSAViewMsg SSAView.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Init ->
            ssaUpdate (SSAView.Show sample) model
        SSAViewMsg m ->
            ssaUpdate m model


ssaUpdate : SSAView.Msg -> Model -> (Model, Cmd Msg)
ssaUpdate m model =
    let
        (cm, cc) = SSAView.update m model.ssa
    in
        ({model | ssa = cm}, Cmd.map SSAViewMsg cc )



init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.succeed Init |> Task.perform identity )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        css = Html.node "style" [type_ "text/css"]
                  [text """@import url("bootstrap.css");
body {  font-family: "Fira Code", Monaco, Courier New; font-size: 13px; }
table.ssa-table { width: 100%; }
.kind-unknown-td { color: #999; font-size: 0.8em; }

thead .block-header td { border-top: 2px solid #ccc; }
thead .block-header-th th { font-size: 0.8em; color: #999; font-weight: normal; }

thead .block-header h5 { margin:0; padding:3px 5px; backround: #cfc; padding-top:2em; }
thead .block-header .code { color: #aaa; font-weight: normal; vertical-align:bottom; padding-top:2.5em; }

tbody tr.block-symbols-exported td.op-code,
tbody tr.block-entry.block-entry-phi td.op-code,
.block-entry .code, .block-entry .op-code,
.block-exit .code, .block-exit .op-code { background: none; font-weight: normal; color: #aaa;  }
.block-entry-local .code { color: #a60; }
.block-entry-global .code { color: #f70; }
.block-exit .code {}

.block-entry .code, .block-entry .op-code { padding-left: 2em; }


thead:hover,
tbody:hover { background: #ffe; }
tbody:hover tr:hover td { background: #ff0; }


td.op-code,
td.code { border-top: 1px dotted #dcb; border-left: 3px solid #555; color: #543; font-weight: bold; padding-left: 5em; width: 50%; }

td.op-kind { background: #ccc; color: #999; font-size: 0.8em; }
td.op-name { text-align:right; font-weight: bold; }
td.op-left { text-align: right; }
td.op-op { text-align: center; font-weight: bold; }
td.op-type { font-style: italic; color: #765; }


tbody tr.block-input td { background-color: #cfe; }
tbody tr.block-entry td { background-color: #fc9; }
tbody tr.block-exit td { background-color: #efc; }
tbody tr.block-symbols-exported td { background-color: #eff; }
tbody tr.block-symbols-exported td.op-code { color: #aaa; font-weight:normal;}



/* --===-=-=-=-=-=-=-=-= */

.code-view { position: absolute; left: 0; right:20em; background: #eee; overflow:scroll; top: 0; bottom:0; }
.step-view { position: absolute; width: 20em; right: 0; }


/* --===-=-=-=-=-=-=-=-= */

.step-enabled,
.step-disabled { cursor: pointer; }

.step-enabled:hover,
.step-disabled:hover { background: #ddd; }

.step-disabled { color: #aaa; font-style: italic; }
.step-enabled { color: #555; }


.node-base-block {  background: #fff; padding: 0.5em 1em; margin: 2em; box-shadow: 1em 1em 1em rgba(0,0,0,0.05); }
                  """
                  ]

        split a b =
            Html.table []
                [ Html.tbody []
                    [ Html.tr []
                        [ Html.td [] [a]
                        , Html.td [] [b]
                        ]
                    ]
                ]

     in
        Html.div []
            [   css
            ,   Html.map SSAViewMsg <| SSAView.view model.ssa
            ]



--    case (compile model.code) of
--        Ok c -> codeView c
--        Err e -> Html.ol [] (List.map warningView e)
