module Css.Padding exposing (..)
{-| Describe me please...
-}

import Css.Attribute exposing (Attribute, attribute)
import Css.Rectangle exposing (Rectangle, SizeRectangle)
import Css.Size exposing (Size)

type alias Padding = SizeRectangle

sizeToS = Css.Size.toS

toAttribute : Padding -> List Attribute
toAttribute p = Css.Rectangle.sizeToAttributes "padding" p


empty : Padding
empty = Css.Rectangle.empty