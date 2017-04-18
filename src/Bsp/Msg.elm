module Bsp.Msg exposing (..)

{-| Describe me please...
-}

-- MSG -------------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor)
import Bsp.SplitView exposing (Direction)


type alias Id =
    Int


type Msg msg local
    = ChildMsg Id msg
    | Select Cursor
    | DeleteAt Cursor
    | SplitAt Cursor Direction local
    | SetLayoutEditingMode LayoutEditingMode
    | SetDirection Cursor Direction
    | SwapLR Cursor
    | Rotate Bsp.SplitView.RotateDirection Cursor
    | RotateParent Bsp.SplitView.RotateDirection Cursor



-- LAYOUT EDITING MODES --------------------------------------------------------


{-| What kind of layout editing mode are we in
-}
type LayoutEditingMode
    = NotEditingLayout
    | EditingLayoutBlocks
