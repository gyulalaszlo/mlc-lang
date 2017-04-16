module Bsp.Test exposing (main)
{-| Describe me please...
-}
import Bsp.Root exposing (LocalModel, Model, css, modelFrom, subscriptions, update, view)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical))
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import MLC.Cursor exposing (Cursor)
import Task

type alias Msg = Bsp.Root.Msg ChildMsg ChildModel
--type alias Model = Bsp.Root.Model ChildMsg ChildModel Int

--init : ( Model, Cmd Msg )



init =
    ( modelFrom bspTraits 0, Cmd.none )
    -- ( initialModel, Task.perform INIT_MSG (Task.succeed MSG_DATA) )


withCss : String -> (model -> Html msg) -> model -> Html msg
withCss css view model =
    Html.div []
        [ Html.node "style"
            [ Html.Attributes.type_ "text/css"]
            [ Html.text css ]

        , view model
        ]


--main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = withCss (css ++ childViewCss) view
        , update = update
        , subscriptions = subscriptions
        }

type ChildMsg = Noop

type ChildModel
    = A
    | B

bspTraits =
    { subscriptions = always Sub.none
    , update = childUpdate
    , view = childView

    , empty = empty
    , leafToolbar = leafToolbar
    , splitToolbar = splitToolbar
    }


-- VIEW: childView


childUpdate : ChildMsg -> LocalModel ChildModel Int -> (LocalModel ChildModel Int, Cmd ChildMsg)
childUpdate msg  model =
    let {local,shared} = model
    in case local of
        A -> (model, Cmd.none)
        B -> (model, Cmd.none)


{-| child view
-}
childView : LocalModel ChildModel Int -> Html ChildMsg
childView {local, shared} =
    case local of
        A ->
            Html.div [] [ Html.text  "AAAA" ]

        B ->
            Html.div [] [ Html.text "BBBB" ]




--empty : Cursor -> Int -> Html Msg
empty cursor _ =
    Html.div []
        [ Html.text "Nothing"
        , Html.button [ onClick <| Bsp.Root.SplitAt cursor Horizontal A ] [ text "-> A" ]
        , Html.button [ onClick <| Bsp.Root.SplitAt cursor Horizontal B ] [ text "-> B" ]
        ]


btn cursor direction v label =
    Html.span
        [ class "btn",  onClick <| Bsp.Root.SplitAt cursor direction v ]
        [ text label ]


hbtn cursor = btn cursor Horizontal
vbtn cursor = btn cursor Vertical

--leafToolbar : (ChildMsg -> Msg) -> Cursor -> ChildModel -> Int -> Html Msg
leafToolbar msg c local _ =
     Html.div []
        [ Html.button [onClick <| Bsp.Root.Select c] [ text <| "LEAF:" ++ toString c ]
        , hbtn c A "|| A"
        , hbtn c B "|| B"
        , vbtn c A "-- A"
        , vbtn c B "-- B"
        , Bsp.Root.parentCursor c
            |> Maybe.map (\cc ->
                Html.button [onClick <| Bsp.Root.Select cc ] [ text <| "Parent" ++ toString cc  ])
            |> Maybe.withDefault (text "")
        ]

--splitToolbar : Cursor -> Int ->  Html Msg
splitToolbar cursor _ = Html.text ""


{-| CSS parts for childView
-}
childViewCss : String
childViewCss = """
.child-view {  }

.btn { display:inline-block; padding: 0.3em 1em; cursor: pointer; font-size:0.8em;}
.btn:hover { text-decoration:underline; }

.bsp-root-view { background: black; position: absolute; left: 0; right:0; top:0; bottom:0; }

.bsp-view-split-wrapper-leaf { background-color: #242527; color: #ccc; border-radius:1em; margin:2px;  }
.bsp-view-split-wrapper-leaf-selected {  }

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


.bsp-view-split-wrapper-node { border-radius: 1em; }
.bsp-view-split-wrapper-selected-node {}
.bsp-view-split-wrapper-selected-node-horizontal {}
.bsp-view-split-wrapper-selected-node-vertical {}
.bsp-view-split-wrapper-node-horizontal {}
.bsp-view-split-wrapper-node-vertical {}

.bsp-view-split-wrapper-node-vertical-selected,
.bsp-view-split-wrapper-node-horizontal-selected { background: #f70; }
"""


