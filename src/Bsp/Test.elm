module Bsp.Test exposing (main)
{-| Describe me please...
-}
import Bsp.Cursor exposing (parentCursor)
import Bsp.DefaultTheme
import Bsp.RootModel exposing (LayoutEditingMode(EditingLayoutBlocks), LocalModel, Model, Msg(..), modelFrom)
import Bsp.Root exposing (subscriptions, update, view)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical))
import Colors.Monokai
import Css
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import MLC.Cursor exposing (Cursor)
import Task

type alias Msg = Bsp.RootModel.Msg ChildMsg ChildModel
type alias Model = LocalModel ChildMsg ChildModel Int
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
        , view = withCss Bsp.DefaultTheme.css view
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
--    , leafToolbar = leafToolbar
--    , splitToolbar = splitToolbar
    , toolbars = Bsp.DefaultTheme.toolbarTraits childLabel
    }


-- VIEW: childView


childUpdate : ChildMsg -> Model -> (ChildModel, Int, Cmd Msg)
childUpdate msg  model =
    let {local,shared} = model
    in case local of
        A -> (local, shared, Cmd.none)
        B -> (local, shared, Cmd.none)


{-| child view
-}
childView : Model -> Html Msg
childView {local, shared, cursor, msg} =
    case local of
        A ->
            Html.div []
                [ Html.text  "AAAA"
                , leafToolbar cursor
                ]

        B ->
            Html.div []
                [ Html.text  "BBBB"
                , leafToolbar cursor
                ]




--empty : Cursor -> Int -> Html Msg
empty cursor _ =
    Html.div []
        [ Html.text "Nothing"
        , Html.button [ onClick <| SplitAt cursor Horizontal A ] [ text "-> A" ]
        , Html.button [ onClick <| SplitAt cursor Horizontal B ] [ text "-> B" ]
        ]


btn cursor direction v label =
    Html.button
        [ onClick <| SplitAt cursor direction v ]
        [ text label ]


hbtn cursor = btn cursor Horizontal
vbtn cursor = btn cursor Vertical

--leafToolbar : (ChildMsg -> Msg) -> Cursor -> ChildModel -> Int -> Html Msg
leafToolbar c =
     Html.div []
        [ Html.button [onClick <| Select c] [ text <| "SELECT" ]
        , Html.button [onClick <| SetLayoutEditingMode EditingLayoutBlocks] [ text <| "LAYOUT" ]
        , hbtn c A "|| A"
        , hbtn c B "|| B"
        , vbtn c A "-- A"
        , vbtn c B "-- B"
--        , parentCursor c
--            |> Maybe.map (\cc ->
--                Html.button [onClick <| Select cc ] [ text <| "Parent" ++ toString cc  ])
--            |> Maybe.withDefault (text "")
        ]


childLabel : ChildModel -> String
childLabel m =
    case m of
        A -> "A"
        B -> "B"
