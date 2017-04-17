module Bsp.SplitView exposing
    ( SplitModel(..)
    , Direction(..), Ratio(..), SplitMeta

    , directionToString

--    , Msg(..)

--    , subscriptions
--    , update
--    , view

    , foldViews, flipNode

    , empty, leaf
    , binary, horizontal, vertical

    , splitAtCursor, swapAtCursor, setDirectionAtCursor, rotateAtCursor

    , RotateDirection(..)
--    , css
    )
{-| Describe me please...
-}

import Bsp.Cursor exposing (BspStep(Left, Right), Cursor(..))
import Color exposing (Color)
--import Dict exposing (Dict)
import Css exposing (..)
import Error exposing (Error)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)


-- MODEL

type Direction
    = Horizontal
    | Vertical


directionToString : Direction -> String
directionToString d =
    case d of
        Horizontal -> "horizontal"
        Vertical -> "vertical"

{-| Represents a node in a BSP hierarchy
-}
type SplitModel v
    = Node (SplitMeta v)
    | Leaf v
    | Empty




type alias SplitMeta v =
    { a: SplitModel v
    , b: SplitModel v
    , direction: Direction
    , ratio: Ratio
    }

-- MONOID : EMPTY

{-|
-}
empty : SplitModel v
empty = Empty

-- APPLICATIVE : OF

{-| Creates a new leaf node for the BSP tree from an initial view.
-}
leaf : v -> SplitModel v
leaf v = Leaf v

-- SEMIGROUP: CONCAT


{-| Concatenates two nodes into a node
-}
binary : Direction -> Ratio ->  SplitModel v -> SplitModel v -> SplitModel v
binary d r a b = Node { a = a, b = b, direction = d, ratio = r }

horizontal : Ratio -> SplitModel v -> SplitModel v -> SplitModel v
horizontal = binary Horizontal

vertical : Ratio -> SplitModel v -> SplitModel v -> SplitModel v
vertical = binary Vertical



type alias LeafFolder v b = v -> b -> b
type alias NodeFolder b = Direction -> Ratio -> b -> b -> b -> b

foldViews : NodeFolder b -> LeafFolder msg b -> b -> SplitModel msg -> b
foldViews nodeFn leafFn init model =
    case model of
        Leaf v -> leafFn v init
        Node {a, b, direction, ratio} ->
            let recur = foldViews nodeFn leafFn init
            in nodeFn direction ratio (recur a) (recur b) init
        Empty -> init


nodeMeta : SplitModel v -> Maybe (SplitMeta v)
nodeMeta v =
    case v of
        Node n -> Just n
        _ -> Nothing

mapNodeMeta : (SplitMeta v -> SplitMeta v) -> SplitModel v -> Maybe (SplitModel v)
mapNodeMeta fn v =
    nodeMeta v |> Maybe.map (\n -> Node <| fn n)




mapNodeMetaOp : (SplitModel v -> x) -> (SplitMeta v -> SplitMeta v) -> SplitModel v -> Result x (SplitModel v)
mapNodeMetaOp err fn v =
    mapNodeMeta fn v |> Result.fromMaybe (err v)

attemptNodeMetaOp : (SplitModel v -> x) -> (SplitMeta v -> Result x (SplitMeta v)) -> SplitModel v -> Result x (SplitModel v)
attemptNodeMetaOp err fn v =
    nodeMeta v
        |> Result.fromMaybe (err v)
        |> Result.andThen fn
        |> Result.map Node


flipNode : SplitModel v -> Maybe (SplitModel v)
flipNode n =
    mapNodeMeta (\n -> { n | a = n.b, b = n.a, ratio = flipRatio n.ratio }) n




{-| The split ratio for a BSP is pretty simple: either one size is
fixed to some value or both are equal.
-}
type Ratio
    = FixedA CssDimension
    | FixedB CssDimension
    | Equal

flipRatio : Ratio -> Ratio
flipRatio r =
    case r of
        FixedA d -> FixedB d
        FixedB d -> FixedA d
        _ -> r


-- BSP VIEW TREE OPERATIONS ----------------------------------------------------

splitAtCursor : Direction -> Ratio -> v -> Cursor -> SplitModel v -> Result Error (Cursor, SplitModel v)
splitAtCursor direction ratio id cursor node =
    let
        recur cc newNode =
            splitAtCursor direction ratio id cc newNode
    in
        case ( node, cursor ) of
            ( Empty, CHead ) ->
                Ok <| ( cursor, leaf id )

            ( _, CHead ) ->
                Ok <| ( CRight CHead, binary direction ratio node <| leaf id )

            ( Node m, CLeft cc ) ->
                recur cc m.a |> Result.map (\( c, aa ) -> ( CLeft c, Node { m | a = aa } ))

            ( Node m, CRight cc ) ->
                recur cc m.b |> Result.map (\( c, bb ) -> ( CRight c, Node { m | b = bb } ))

            _ ->
                Error.errMsg
                    [ "Cannot traverse cursor:"
                    , toString cursor
                    , "in BSP tree:"
                    , toString node
                    ]

type alias ViewAndCursor v = (Cursor, SplitModel v )
type alias UpdateResult v = Result Error (ViewAndCursor v)
type alias CursorFn = Cursor -> Cursor

{-|
-}
type alias CursorUpdateFn v = CursorFn -> SplitModel v -> UpdateResult v

-- CURSOR FOLDING TRAITS -------------------------------------------------------

stepIntoSplitView : Bsp.Cursor.BspStep -> SplitModel v -> Result Error (SplitModel v, (SplitModel v -> SplitModel v))
stepIntoSplitView dir v =
    case (v, dir) of
        (Node n, Left) -> Ok (n.a, (\aa -> Node { n | a = aa }))
        (Node n, Right) -> Ok (n.b, (\bb -> Node { n | b = bb }))
        _ -> Error.errMsg ["Cannot step in direction", toString dir, "into", toString v]


stepIntoViewAndCursor : Bsp.Cursor.BspStep -> ViewAndCursor v -> Result Error (ViewAndCursor v, (ViewAndCursor v -> ViewAndCursor v))
stepIntoViewAndCursor dir r =
        let
            innerMap ff c v =
                Ok ((c, v), (\(cc, vv) -> ff cc vv))

            fn (c,v) = case (v, c, dir) of

                (Node n, CLeft c, Left) ->
                    innerMap (\cc aa -> (CLeft cc, Node { n | a = aa})) c n.a

                (Node n, CRight c, Right) ->
                    innerMap (\cc bb -> (CRight cc, Node { n | b = bb})) c n.b

                _ -> Error.errMsg ["Cannot step in direction", toString dir, "into", toString v]
        in
            fn r
            |> Debug.log "stepInto"


-- OPERATIONS WITH CURSORS -----------------------------------------------------


swapAtCursor : Cursor -> SplitModel v -> Result Error (SplitModel v)
swapAtCursor c v =
    let
        err c v = Error.makeMsg ["Cannot flip node at:", toString c, "in", toString v]
        flipFn v = flipNode v |> Result.fromMaybe (err c v)
    in
        Bsp.Cursor.foldCursor flipFn stepIntoSplitView v c

setDirectionAtCursor : Direction -> Cursor -> SplitModel v -> Result Error (SplitModel v)
setDirectionAtCursor d c v =
    let
        err c v = Error.makeMsg ["Cannot flip node at:", toString c, "in", toString v]
        fn v = mapNodeMetaOp (err c) (\n -> { n | direction = d }) v
    in
        Bsp.Cursor.foldCursor fn stepIntoSplitView v c


{-| Rotate changes the views without modifying the splits on their level
-}
type RotateDirection
    = CW
    | CCW



rotate : RotateDirection -> SplitMeta v -> Result Error (SplitMeta v)
rotate dir n =
    let
        err = Error.errMsg ["Cannot rotate node", toString n]

        update n aa bb = Node { n | a = aa, b = bb }

    in case (dir, n.a, n.b) of
        (CW, Node na, Node nb) ->
            Ok { n
                | a = update na na.b nb.b
                , b = update nb na.a nb.a
                }
        (CW, Node na, b) ->
            Ok { n
                | a = update na na.b b
                , b = na.a
                }
        (CW, a, Node nb) ->
            Ok { n
                | a = nb.b
                , b = update nb a nb.a
                }

        (CCW, Node na, Node nb) ->
            Ok { n
                | a = update na nb.a na.a
                , b = update nb nb.b na.b
                }
        (CCW, Node na, b) ->
            Ok { n
                | a = update na b na.a
                , b = na.b
                }
        (CCW, a, Node nb) ->
            Ok { n
                | a = nb.a
                , b = update nb nb.b a
                }
        _ -> Error.errMsg ["Cannot rotate node", toString n]


rotateAtCursor : RotateDirection -> Cursor -> SplitModel v -> UpdateResult v
rotateAtCursor dir c v =
    let
        err c v = Error.makeMsg ["Cannot find node at:", toString c, "in", toString v]

        fn (cc,v) =
            attemptNodeMetaOp (err c) (rotate dir) v
                |> Result.map  (\v -> (cc, v))
    in
        Bsp.Cursor.foldCursor fn stepIntoViewAndCursor (c,v) c








nodeToS n = case n of
    Leaf v -> toString v
    Node {a,b} -> "[ " ++ nodeToS a ++ ", " ++ nodeToS b ++ " ]"
    Empty -> "<Empty>"

debugNode n = let dd = Debug.log (nodeToS n) "" in n