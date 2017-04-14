module Css.Size exposing (..)

{-| Describe me please...
-}


{-| A single dimension size (like 1px or 50% or 0.2em).
-}
type Size
    = Pixel Float
    | Percent Float
    | Em Float
    | Zero


toS : Size -> String
toS d =
    case d of
        Pixel px ->
            toString px ++ "px"

        Percent pc ->
            toString pc ++ "%"

        Em pc ->
            toString pc ++ "em"

        Zero ->
            "0"


negate : Size -> Size
negate d =
    case d of
        Pixel v ->
            Pixel -v

        Percent v ->
            Percent -v

        Em v ->
            Em -v

        Zero ->
            Zero


full : Size
full =
    Percent 100


zero : Size
zero =
    Zero


px : Float -> Size
px =
    Pixel


pct : Float -> Size
pct =
    Percent


em : Float -> Size
em =
    Em
