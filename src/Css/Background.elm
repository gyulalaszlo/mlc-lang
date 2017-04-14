module Css.Background exposing (..)

{-| Describe me please...
-}

import Color exposing (Color)
import Css.Attribute exposing (Attribute, attribute, colorToString)


type Background
    = Auto
    | Solid Color


toAttribute : Background -> List Attribute
toAttribute bg =
    case bg of
        Auto ->
            []

        Solid c ->
            [ attribute "background-color" [ colorToString c ] ]


empty =
    Auto
