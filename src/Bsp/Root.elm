module Bsp.Root
    exposing
        ( subscriptions
        , update
        , view
        )

{-| Describe me please...
-}

import Array exposing (Array)
import Bsp.Cursor exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Bsp.Model exposing (..)
import Bsp.Msg exposing (..)
import Bsp.Traits exposing (..)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), SplitModel(Empty, Leaf, Node), Ratio, SplitMeta, binary, directionToString, leaf)
import Dict exposing (Dict)
import Error exposing (Error)
import Html.Events exposing (onClick)
import List.Extra


-- SUBSCRIPTIONS


subscriptions : Model m l s -> Sub (Msg m l)
subscriptions model =
    Sub.none



-- UPDATE


update : Msg m l -> Model m l s -> ( Model m l s, Cmd (Msg m l) )
update msg model =
    case msg of
        ChildMsg id m ->
            model ! []

        Select c ->
            { model | cursor = c } ! []

        SplitAt c d l ->
            ( insertAt c d Bsp.SplitView.Equal l model
            , Cmd.none
            )

        SetLayoutEditingMode m ->
            { model | layoutEditingMode = m } ! []

        SwapLR c ->
            Bsp.SplitView.swapABAtCursor c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        SetDirection c d ->
            Bsp.SplitView.setDirectionAtCursor d c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        Rotate d c ->
            fromUpdateResult (Bsp.SplitView.rotateAtCursor d) c model

        RotateParent d c ->
            case Bsp.Cursor.parentCursor c of
                Nothing ->
                    ( model, Cmd.none )

                Just cc ->
                    fromUpdateResult (Bsp.SplitView.rotateAtCursor d) cc model

        DeleteAt c ->
            fromUpdateResult Bsp.SplitView.deleteAtCursor c model


{-| do an `update()` using an update function
-}
fromUpdateResult : (Cursor -> SplitModel Id -> Result Error ( Cursor, SplitModel Id )) -> Cursor -> Model m l s -> ( Model m l s, Cmd (Msg m l) )
fromUpdateResult updateFn cursor model =
    updateFn cursor model.rootView
        |> Result.map (\( c, v ) -> ( { model | rootView = v, cursor = c }, Cmd.none ))
        |> Result.withDefault ( model, Cmd.none )



-- VIEW


view : Model m l s -> Html (Msg m l)
view model =
    let
        globalToolbar =
            case model.layoutEditingMode of
                NotEditingLayout ->
                    model.traits.toolbars.normal

                EditingLayoutBlocks ->
                    model.traits.toolbars.layoutEditing
    in
        div [ class "bsp-root-view" ]
            [ globalToolbar.global model.shared (localModelAt model.cursor model)
            , splitWrapper [ "root" ] [ treeSubView model identity model.rootView ]
            ]



-- VIEW: treeSubView


{-| the view of a tree node or leaf or empty node
-}
treeSubView : Model m l s -> (Cursor -> Cursor) -> SplitModel Id -> Html (Msg m l)
treeSubView model cursorFn node =
    let
        recur =
            treeSubView model

        cursor =
            cursorFn CHead

        wrapper cls els =
            wrapperWithSelection model cursor cls els

        nodeViewBaseTraits =
            nodeViewBaseTraitsFor cursor model
    in
        case node of
            Bsp.SplitView.Node meta ->
                let
                    { a, b, direction, ratio } =
                        meta

                    ( l, r ) =
                        splitAttrs direction ratio


                    nodeDiv v style dir el =
                        div [ prefixedClasses "node" [ "split", directionToString direction, v ]
                            , style
                            ]
                            [ recur (cursorFn << dir) el ]
                in
                    nodeViewBaseTraits.split (sharedModelFor cursorFn meta model.shared) <|
                        wrapper [ "node", directionToString direction ] <|
                            [ nodeDiv "a" l CLeft a
                            , nodeDiv "b" r CRight b
                            ]

            Bsp.SplitView.Leaf id ->
                wrapper [ "leaf" ]
                    [ localModelFor cursor id model
                        |> Maybe.map (\mdl -> nodeViewBaseTraits.leaf model.traits.view mdl)
                        |> Maybe.withDefault (text "Cannot find local for view")
                    ]

            Bsp.SplitView.Empty ->
                wrapper [ "empty" ]
                    [ nodeViewBaseTraits.empty cursor model.shared
                    ]



-- WRAPPERS FOR BSP PANES ------------------------------------------------------


{-| Wraps a BSP view and adds the `selected` class bit if its selected
-}
wrapperWithSelection : Model m l s -> Cursor -> List String -> List (Html msg) -> Html msg
wrapperWithSelection model cursor classArgs els =
    let
        classBits =
            classArgs
                ++ [ if (model.cursor == cursor) then
                        "selected"
                     else
                        "not-selected"
                   , case model.layoutEditingMode of
                       EditingLayoutBlocks -> "editing"
                       NotEditingLayout -> "not-editing"
                   ]

    in
        splitWrapper classBits els


{-| Wraps all BSP views with proper class names
-}
splitWrapper : List String -> List (Html msg) -> Html msg
splitWrapper classArgs els =
    div
        [ prefixedClasses "bsp-view-split-wrapper" classArgs
        , style
            [ ( "position", "absolute" )
            , ( "overflow", "hidden" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            ]
        ]
        els



-- VIEW: splitView


prefixedClasses : String -> List String -> Html.Attribute msg
prefixedClasses s ss =
    let prefix v = s ++ "-" ++ v
    in class <| String.join " " <| s :: (List.map prefix ss)

classes : List String -> Html.Attribute msg
classes ss =
    class <| String.join " " (List.map (String.join "-") <| List.Extra.inits ss)


bspClasses : String -> List String -> Html.Attribute msg
bspClasses static ss =
    classes <| ("bsp-view-" ++ static) :: ss


bspClassesFor : String -> List String -> List String -> Html.Attribute msg
bspClassesFor static prefixes ss =
    bspClasses static <| prefixes ++ ss


splitSize : Ratio -> ( Float, Float )
splitSize r =
    case r of
        Bsp.SplitView.Equal ->
            ( 50, 50 )

        _ ->
            ( 50, 50 )


{-| Returns the 'style' attribute for the left and right side for the given
Direction and Ratio.
-}
splitAttrs : Direction -> Ratio -> ( Html.Attribute msg, Html.Attribute msg )
splitAttrs direction ratio =
    let
        pct s v =
            ( s, toString v ++ "%" )

        pcts vv =
            List.map2 pct [ "left", "right", "top", "bottom" ] vv

        rect vv =
            ( "position", "absolute" ) :: (pcts vv)

        attrs fn =
            splitSize ratio
                |> fn
                |> (\( a, b ) -> ( style <| rect a, style <| rect b ))
    in
        case direction of
            Horizontal ->
                attrs (\( a, b ) -> ( [ 0, b, 0, 0 ], [ a, 0, 0, 0 ] ))

            Vertical ->
                attrs (\( a, b ) -> ( [ 0, 0, 0, b ], [ 0, 0, a, 0 ] ))
