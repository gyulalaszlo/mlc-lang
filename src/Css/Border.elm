module Css.Border exposing (..)
{-| Describe me please...
-}

import Color exposing (Color)
import Css.Attribute exposing (Attribute)
import Css.Rectangle exposing (Rectangle)
import Css.Size exposing (Size)


type BorderAttributes
    = WidthAndType Size BorderType
    | WidthAndTypeAndColor Size BorderType Color

type BorderType
    = Solid
    | Dotted

type alias Border = Rectangle BorderAttributes


toAttributes: Border -> List Attribute
toAttributes b = Css.Rectangle.toAttributes borderToString "border" b


borderToString : BorderAttributes -> String
borderToString b =
    case b of
        WidthAndType w t ->
            String.join " " [Css.Size.toS w, borderTypeToString t]
        WidthAndTypeAndColor w t c ->
            String.join " " [Css.Size.toS w, borderTypeToString t, Css.Attribute.colorToString c]

borderTypeToString : BorderType -> String
borderTypeToString b =
    case b of
        Solid -> "solid"
        Dotted -> "dotted"

empty = Css.Rectangle.empty