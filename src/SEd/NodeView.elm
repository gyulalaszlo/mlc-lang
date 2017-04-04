module SEd.NodeView exposing
    ( Model(..) , initialModel

    , Msg(..)
    , subscriptions, update, view

    , NodeSelection(..)

    , leaf, node, leafMeta, nodeMeta
    , LeafMeta, NodeMeta)
{-| Describe me please...
|-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


-- MODEL


type Model
    = Node { meta: NodeMeta,  children: List Model }
    | Leaf { meta: LeafMeta }

type NodeSelection
    = IsTarget
    | IsInPath
    | NotSelected

type alias NodeMeta = { selection: NodeSelection }
type alias LeafMeta = { isSelected: Bool, label: String }


initialModel : Model
initialModel =
    Leaf { meta = leafMeta }

leafMeta : LeafMeta
leafMeta = { isSelected = False, label = "" }

nodeMeta : NodeMeta
nodeMeta = { selection = NotSelected }

leaf : LeafMeta -> Model
leaf m = Leaf { meta = m }

node : NodeMeta -> List Model -> Model
node m c = Node { meta = m, children = c }

-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []


-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Leaf {meta} -> leafView meta
        Node {meta, children} -> nodeView meta children
--    exprView model
--    div [ class "NodeView-view" ]
--        [ text <| toString model ]


leafView : LeafMeta -> Html Msg
leafView meta =
    Html.span [] [ text <| "leaf:" ++ toString meta ]

nodeView : NodeMeta -> List Model -> Html Msg
nodeView meta children =
    Html.div []
        [ Html.h4 [] [ text "List" ]
        , Html.ul [] <|
            List.map (\c -> Html.li [] [ view c ]) children
        ]



