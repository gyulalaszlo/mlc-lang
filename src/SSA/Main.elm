{-

   SSA.Main
   --------

   Main module for SSA view

-}


module SSA.Main exposing (..)

import Codegen.Indented exposing (applyIndents)
import Dict
import GraphLike.EdgeReduce exposing (mapNodesToList)
import Html.Events exposing (onClick)
import Pages.GraphLikeView exposing (graphLikeView)
import Pages.Optimizer as Optimizer
import SSA.Types exposing (BlockGraph, BlockTree)
import Helpers.Attributes
import Html exposing (Html, div, td, text, tr)
import Html.Attributes exposing (class, colspan, rowspan, style)
import SSA.InstructionsTable exposing (blockView, instructionsTable)
import Json.Encode
import SSA.SSAForm as SSAForm exposing (..)


------------------------------------------------------

type PageShown
    = BlocksView
    | GraphView
    | JsonView


type Code
    = HasCode BlockTree
    | HasNoCode

type alias Model =
    { code: Code
    , shown: PageShown
    , optimizer: Optimizer.Model
    }


initialModel =
    { code = HasNoCode
    , shown = GraphView
    , optimizer = Optimizer.initialModel
    }



type Msg
    = Show BlockTree
    | ShowPage PageShown
    | OptimizerMsg Optimizer.Msg



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Show blocks ->
            ( { model | code =  HasCode blocks }, Cmd.none )

        ShowPage p -> ({ model | shown = p }, Cmd.none )

        OptimizerMsg m ->
            let
                (sm, sc) = Optimizer.update m model.optimizer
            in
                ({model | optimizer = sm}, Cmd.map OptimizerMsg sc)


view : Model -> Html Msg
view {shown, code, optimizer} =
        case code of
            HasNoCode ->
                Html.text "No SSA code"

            HasCode blocks ->
                div [class "compiler-view"]
                    [ div
                        [class "step-view"]
                        [ Html.map OptimizerMsg <| Optimizer.view optimizer
                        ]

                    , div
                        [class "code-view"]
                        [ headerView
                        , pageView shown blocks optimizer
                        ]
                    ]


headerView : Html Msg
headerView =
    div
        [ class "json-head" ]
        [ Html.button [ onClick (ShowPage JsonView) ] [ text "Show JSON" ]
        , Html.button [ onClick (ShowPage BlocksView) ] [ text "Show Blocks" ]
        , Html.button [ onClick (ShowPage GraphView) ] [ text "Show Graph" ]
        ]

pageView : PageShown -> BlockTree -> Optimizer.Model -> Html Msg
pageView s bs optimizer =
    let
        optimized = Optimizer.runSteps optimizer bs
    in
        case s of
            JsonView -> jsonView bs
            BlocksView ->  div [] <| mapNodesToList instructionsTable optimized.graph
            GraphView -> graphLikeView optimized



jsonView : BlockTree -> Html Msg
jsonView  b =
        div [ class "block-json" ]
            [ Html.pre []
                [ Html.code []
                    [ text <| toString b ]
                ]
            ]



