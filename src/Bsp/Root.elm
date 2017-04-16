module Bsp.Root
    exposing
        ( Model
        , LocalModel
        , Cursor, parentCursor
        , modelFrom
        , Msg(..)
        , subscriptions
        , update
        , view
        , css
        )

{-| Describe me please...
-}

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), Model(Empty, Leaf, Node), Ratio, SplitMeta, binary, leaf)
import Dict exposing (Dict)
import Error exposing (Error)
import Html.Events exposing (onClick)
import List.Extra



-- ID --------------------------------------------------------------------------


type alias Id =
    Int


nextId : Id -> Id
nextId old =
    old + 1




-- MODEL -----------------------------------------------------------------------


type alias LocalModel local shared =
    { local : local
    , shared : shared
    }


type alias Model msg local shared =
    { shared : shared
    , locals : Dict Id local
    , traits : Traits msg local shared
    , rootView : Bsp.SplitView.Model Id
    , cursor : Cursor
    , nextId : Id
    }


type alias Traits msg local shared =
    { subscriptions : LocalModel local shared -> Sub msg
    , update : msg -> LocalModel local shared -> ( LocalModel local shared, Cmd msg )
    , view : LocalModel local shared -> Html msg
    , empty : Cursor -> shared -> Html (Msg msg local)
    , leafToolbar : (msg -> Msg msg local) -> Cursor -> local -> shared -> Html (Msg msg local)
    , splitToolbar : Cursor -> shared -> Html (Msg msg local)
    }


modelFrom : Traits m l s -> s -> Model m l s
modelFrom traits shared =
    { shared = shared
    , locals = Dict.empty
    , traits = traits
    , rootView = Bsp.SplitView.Empty
    , cursor = CHead
    , nextId = 0
    }


type alias ViewNode =
    Bsp.SplitView.Model Id


type Cursor
    = CLeft Cursor
    | CRight Cursor
    | CHead


parentCursor : Cursor -> Maybe Cursor
parentCursor c =
    case c of
        CLeft (CHead) -> Just CHead
        CRight (CHead) -> Just CHead
        CLeft cc -> parentCursor cc |> Maybe.map CLeft
        CRight cc -> parentCursor cc |> Maybe.map CRight
        _ -> Nothing


splitAtCursor : Direction -> Ratio -> Id -> Cursor -> ViewNode -> Result Error ViewNode
splitAtCursor direction ratio id cursor node =
    let
        recur cc newNode =
            splitAtCursor direction ratio id cc newNode
    in
        case ( node, cursor ) of
            ( Empty, CHead ) ->
                Ok <| leaf id

            ( _, CHead ) ->
                Ok <| binary direction ratio node <| leaf id

            ( Node m, CLeft cc ) ->
                recur cc m.a |> Result.map (\aa -> Node { m | a = aa })

            ( Node m, CRight cc ) ->
                recur cc m.b |> Result.map (\bb -> Node { m | b = bb })

            _ ->
                Error.errMsg
                    [ "Cannot traverse cursor:"
                    , toString cursor
                    , "in BSP tree:"
                    , toString node
                    ]


insertLocal : l -> Model m l s -> Model m l s
insertLocal local model =
    let
        id =
            model.nextId
    in
        { model
            | nextId = nextId id
            , locals = Dict.insert id local model.locals
        }


insertAt : Cursor -> Direction -> Ratio -> l -> Model m l s -> Model m l s
insertAt cursor direction ratio local model =
    splitAtCursor direction ratio model.nextId cursor model.rootView
        |> Result.map (\newRoot -> { model | rootView = newRoot })
        |> Result.map (insertLocal local)
        |> Result.withDefault model



-- MSG


type Msg msg local
    = ChildMsg Id msg
    | Select Cursor
    | SplitAt Cursor Direction local



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



-- VIEW


view : Model m l s -> Html (Msg m l)
view model =
    div [ class "bsp-root-view" ]
        [ treeSubView model identity model.rootView
        ]



-- VIEW: treeSubView


{-| the view of a tree node or leaf or empty node
-}
treeSubView : Model m l s -> (Cursor -> Cursor) -> Bsp.SplitView.Model Id -> Html (Msg m l)
treeSubView model cursorFn node =
    let
        recur =
            treeSubView model

        cursor =
            cursorFn CHead

        traits =
            model.traits

        shared =
            model.shared


        classAttrs cs = List.concat
            [ cs
            , if (model.cursor == cursor) then ["selected"] else []
            ]
        wrapper classes els = splitWrapper (classAttrs classes) els
    in
            case node of
                Bsp.SplitView.Node meta ->
                    wrapper
                        ["node"
                        , String.toLower <| toString <| meta.direction]
                    <| splitView model cursorFn meta

                Bsp.SplitView.Leaf id ->
                    wrapper ["leaf"] <|
                        [ Dict.get id model.locals
                            |> Maybe.map (\local -> traits.leafToolbar (ChildMsg id) cursor local shared)
                            |> Maybe.withDefault (text "Cannot find local for view")
                        , text <| toString id
                        ]

                Bsp.SplitView.Empty ->
                    wrapper ["empty"]
                        [ traits.empty cursor model.shared
                        ]


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


{-| CSS parts for treeSubView
-}
treeSubViewCss : String
treeSubViewCss =
    """
.tree-sub-view {  }
"""



-- VIEW: splitView

classes : List String -> Html.Attribute msg
classes ss =
    class <| String.join " " (List.map (String.join "-") <| List.Extra.inits ss)

bspClasses : String -> List String -> Html.Attribute msg
bspClasses static ss = classes <| ( "bsp-view-" ++ static ) :: ss

bspClassesFor : String -> List String -> List String -> Html.Attribute msg
bspClassesFor static prefixes ss = bspClasses static <| prefixes ++ ss

{-| split view
-}
splitView : Model m l s -> (Cursor -> Cursor) -> SplitMeta Id -> List (Html (Msg m l))
splitView model cursorFn { a, b, ratio, direction } =
    let
        recur =
            treeSubView model

        cursor =
            cursorFn CHead

        ( l, r ) =
            splitAttrs direction ratio

        classFor = bspClassesFor "node" ["split", String.toLower <| toString direction]
    in
        [ div [ classFor ["a"], l ] [ recur (cursorFn << CLeft) a ]
        , div [ classFor ["b"], r ] [ recur (cursorFn << CRight) b ]
        , div [ classFor ["toolbar"] ] [ model.traits.splitToolbar cursor model.shared ]
        ]


{-| CSS parts for splitView
-}
splitViewCss : String
splitViewCss =
    """
.node-split-view {  }

"""


splitSize : Ratio -> ( Float, Float )
splitSize r =
    case r of
        Bsp.SplitView.Equal ->
            ( 50, 50 )

        _ ->
            ( 50, 50 )


splitAttrs : Direction -> Ratio -> ( Html.Attribute msg, Html.Attribute msg )
splitAttrs direction ratio =
    let
        pct s v =
            ( s, toString v ++ "%" )

        pcts ss vv =
            List.map2 pct ss vv

        rect vv =
            ( "position", "absolute" ) :: (pcts [ "left", "right", "top", "bottom" ] vv)

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






-- CSS


css : String
css =
    """
""" ++ treeSubViewCss ++ splitViewCss
