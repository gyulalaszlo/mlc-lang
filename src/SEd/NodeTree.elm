module SEd.NodeTree exposing (Model, initialModel, Msg(..), subscriptions, update, view, setRoot)
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import SEd.NodeView as NodeView

-- MODEL


type alias Model =
    { nodeView: NodeView.Model
    }


initialModel : Model
initialModel =
    { nodeView = NodeView.initialModel

    }


setRoot : NodeView.Model -> Model -> Model
setRoot n model = { model | nodeView = n }


-- MSG


type Msg
    = Noop
    | NodeViewMsg NodeView.Msg





-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []
        NodeViewMsg m ->
            let
                (sm, sc) = NodeView.update m model.nodeView
            in
                ({ model | nodeView = sm }, Cmd.map NodeViewMsg sc)




-- VIEW


view : Model -> Html Msg
view model =
    div [ class "node-tree-view" ]
        [ div [ class "node-tree-view-inner" ]
            [ Html.pre
                [ class "node-tree-pre" ]
                [ Html.code []
                    [ Html.map NodeViewMsg <| NodeView.view model.nodeView ]
                ]

            ]
        ]
