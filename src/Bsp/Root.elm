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
import Bsp.RootModel exposing (..)
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
            Bsp.SplitView.swapAtCursor c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        SetDirection c d ->
            Bsp.SplitView.setDirectionAtCursor d c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        Rotate c d ->
            Bsp.SplitView.rotateAtCursor d c model.rootView
                |> Result.map (\( c, v ) -> ( { model | rootView = v, cursor = c }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )



-- VIEW


view : Model m l s -> Html (Msg m l)
view model =
    case model.layoutEditingMode of
        NotEditingLayout ->
            normalView model

        EditingLayoutBlocks ->
            layoutEditingView model


normalView : Model m l s -> Html (Msg m l)
normalView model =
    div [ class "bsp-root-view" ]
        [ splitWrapper
            [ "root" ]
            [ treeSubView model identity model.rootView ]
        ]


layoutEditingView : Model m l s -> Html (Msg m l)
layoutEditingView model =
    div [ class "bsp-root-view bsp-root-view-edited" ]
        [ model.traits.toolbars.globalLayoutEditor model.cursor model.shared
        , splitWrapper
            [ "root", "edited" ]
            [ layoutEditingTreeSubView model identity model.rootView ]
        ]



-- VIEW: treeSubView


{-| the view of a tree node or leaf or empty node
-}
layoutEditingTreeSubView : Model m l s -> (Cursor -> Cursor) -> SplitModel Id -> Html (Msg m l)
layoutEditingTreeSubView model cursorFn node =
    let
        recur =
            layoutEditingTreeSubView model

        cursor =
            cursorFn CHead

        { traits, shared } =
            model

        isSelected =
            model.cursor == cursor

        wrapper cls els =
            wrapperWithSelection model cursor ("editing-layout" :: cls) els
    in
        case node of
            Bsp.SplitView.Node meta ->
                wrapper [ "node", directionToString meta.direction ] <|
                    layoutEditingSplitView model cursorFn meta

            Bsp.SplitView.Leaf id ->
                localModelFor cursor id model
                    |> Maybe.map
                        (\mdl ->
                            [ (if isSelected then
                                model.traits.toolbars.leafSelectedLayoutEditing
                               else
                                model.traits.toolbars.leafLayoutEditing
                              )
                                cursor
                                id
                                mdl.local
                                model.shared
                            ]
                        )
                    |> Maybe.withDefault [ text "Cannot find view" ]
                    |> wrapper [ "leaf" ]

            Bsp.SplitView.Empty ->
                wrapper [ "empty" ]
                    [ traits.empty cursor model.shared
                    ]


{-| split view
-}
layoutEditingSplitView : Model m l s -> (Cursor -> Cursor) -> SplitMeta Id -> List (Html (Msg m l))
layoutEditingSplitView model cursorFn meta =
    splitViewBase model.traits.toolbars.splitLayoutEditing layoutEditingTreeSubView model cursorFn meta



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

        { traits, shared } =
            model

        wrapper cls els =
            wrapperWithSelection model cursor cls els
    in
        case node of
            Bsp.SplitView.Node meta ->
                wrapper [ "node", directionToString meta.direction ] <|
                    splitView model cursorFn meta

            Bsp.SplitView.Leaf id ->
                wrapper [ "leaf" ]
                    [ localModelFor cursor id model
                        |> Maybe.map (\mdl -> traits.view mdl)
                        |> Maybe.withDefault (text "Cannot find local for view")
                    ]

            Bsp.SplitView.Empty ->
                wrapper [ "empty" ]
                    [ traits.empty cursor model.shared
                    ]


{-| split view
-}
splitView : Model m l s -> (Cursor -> Cursor) -> SplitMeta Id -> List (Html (Msg m l))
splitView model cursorFn meta =
    splitViewBase model.traits.toolbars.split treeSubView model cursorFn meta


{-| split view
-}
splitViewBase :
    ((Cursor -> Cursor) -> SplitMeta Id -> s -> Html (Msg m l))
    -> (Model m l s -> (Cursor -> Cursor) -> SplitModel Id -> Html (Msg m l))
    -> Model m l s
    -> (Cursor -> Cursor)
    -> SplitMeta Id
    -> List (Html (Msg m l))
splitViewBase toolbar recur model cursorFn meta =
    let
        { a, b, ratio, direction } =
            meta

        cursor =
            cursorFn CHead

        ( l, r ) =
            splitAttrs direction ratio

        classFor =
            bspClassesFor "node" [ "split", directionToString direction ]
    in
        [ div [ classFor [ "a" ], l ] [ recur model (cursorFn << CLeft) a ]
        , div [ classFor [ "b" ], r ] [ recur model (cursorFn << CRight) b ]
        , div [ classFor [ "toolbar" ] ] [ toolbar cursorFn meta model.shared ]
        ]



-- WRAPPERS FOR BSP PANES ------------------------------------------------------


{-| Wraps a BSP view and adds the `selected` class bit if its selected
-}
wrapperWithSelection : Model m l s -> Cursor -> List String -> List (Html msg) -> Html msg
wrapperWithSelection model cursor classArgs els =
    let
        classBits =
            if (model.cursor == cursor) then
                classArgs ++ [ "selected" ]
            else
                classArgs
    in
        splitWrapper classBits els


{-| Wraps all BSP views with proper class names
-}
splitWrapper : List String -> List (Html msg) -> Html msg
splitWrapper classArgs els =
    div
        [ bspClasses "split-wrapper" classArgs
        , style
            [ ( "position", "absolute" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            ]
        ]
        els



-- VIEW: splitView


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
