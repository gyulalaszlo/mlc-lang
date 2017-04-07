module Helpers.BSPSplitView exposing
    ( Model
    , Direction(..), Ratio(..)

    , Msg(..)
    
    , subscriptions
    , update
    , view

    , foldViews

    , empty, leaf
    , binary, horizontal, vertical

    , css
    )
{-| Describe me please...
-}

import Color exposing (Color)
import Dict exposing (Dict)
import Helpers.CssBit exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Regex


-- MODEL

type Direction
    = Horizontal
    | Vertical

{-| Represents a node in a BSP hierarchy
-}
type Model msg
    = Node (SplitMeta msg)
    | Leaf (Html msg)
    | Empty




type alias SplitMeta v =
    { a: Model v
    , b: Model v
    , direction: Direction
    , ratio: Ratio
    }

-- MONOID : EMPTY

{-|
-}
empty : Model v
empty = Empty

-- APPLICATIVE : OF

{-| Creates a new leaf node for the BSP tree from an initial view.
-}
leaf : Html msg -> Model msg
leaf v = Leaf v

-- SEMIGROUP: CONCAT


{-| Concatenates two nodes into a node
-}
binary : Direction -> Ratio ->  Model v -> Model v -> Model v
binary d r a b = Node { a = a, b = b, direction = d, ratio = r }

horizontal : Ratio -> Model v -> Model v -> Model v
horizontal = binary Horizontal

vertical : Ratio -> Model v -> Model v -> Model v
vertical = binary Vertical



type alias LeafFolder msg b = (Html msg) -> b -> b
type alias NodeFolder b = Direction -> Ratio -> b -> b -> b -> b

foldViews : NodeFolder b -> LeafFolder msg b -> b -> Model msg -> b
foldViews nodeFn leafFn init model =
    case model of
        Leaf v -> leafFn v init
        Node {a, b, direction, ratio} ->
            let recur = foldViews nodeFn leafFn init
            in nodeFn direction ratio (recur a) (recur b) init
        Empty -> init







{-| The split ratio for a BSP is pretty simple: either one size is
fixed to some value or both are equal.
-}
type Ratio
    = FixedA CssDimension
    | FixedB CssDimension
    | Equal




-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model msg -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model msg -> (Model msg, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []


-- VIEW


view : Model msg -> Html msg
view =
    foldViews splitView leafView <|
        div [ class "BSPSplitView-view" ]
            [ text "NO CONTENT" ]



{-| The frame for a binary split
-}
splitView : Direction -> Ratio -> Html msg -> Html msg -> Html msg -> Html msg
splitView direction ratio a b init =
    case direction of

        Horizontal ->
            let
                kind = "horizontal"

                baseCss = positionAbsolute << top p0 << bottom p0


                ab fns = List.map ((<<) baseCss) fns


                attrs s =
                    case s of
                        FixedA aa ->
                            ab [left p0 << width aa, left aa, left aa << right p0]

                        FixedB bb ->
                            ab [left p0 << right bb, right bb, width bb << right p0]


                        Equal  ->
                            let p50 = Percent 50
                            in ab [left p0 << width p50, left p50, left p50 << width p50]

                -- The inner view of a binary split
                inner view attrs =
                    div [ style <| attrs []
                        , class "binary-inner"
                        ] [view]

                separator =
                    div [class "binary-split-splitter"] []

            in
                List.map2 inner [ a, separator, b ] (attrs ratio)
                    |> binarySplitView "horizontal"


        Vertical ->
            div [ class "binary-split", class "bsp-vertical" ]
                [ a, b ]


leafView : Html msg -> Html msg -> Html msg
leafView v init =
    div [ class "leaf-split" ]
        [ v
        ]

{-| header for a binary split
-}
binarySplitView : String -> List (Html msg) -> Html msg
binarySplitView kind children =
   div  [ class "binary-split"
        , class <| "bsp-" ++ kind
        , style [ ("overflow", "hidden")
                , ("position", "relative")
                , ("width", "100%")
                , ("height", "100%")
                ]
        ]
        children






-- CSS





type alias Theme =
    { normalBackground: Color
    , hoverBackground: Color
    }

{-|
} THe CSS for the splitter
-}
css : Theme -> String
css {hoverBackground, normalBackground} = """

/* BSP */

.binary-split {  }
.binary-split-splitter {
    cursor: pointer;
    z-index: 999;
    background: {{normal-color}};
    position: absolute;
    padding: 2px;
    width:100%;
    height:100%;
    top: 0;
    left:0;

    transition: background 0.2s, padding 0.2s;
}


.binary-split-splitter:hover {
    background: {{hover-color}};
    padding: 3px;
}
"""
    |> templateWith
            (Dict.fromList
                [ ("normal-color", color normalBackground)
                , ("hover-color", color hoverBackground)
                ])
