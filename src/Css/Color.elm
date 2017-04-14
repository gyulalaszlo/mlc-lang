module Css.Color exposing (..)
{-| Describe me please...
-}

import Color exposing (Color)



toS : Color -> String
toS c =
    let {red,green,blue,alpha} = Color.toRgb c
        bits = List.map toString [red,green,blue,alpha]
    in "rgba(" ++ String.join ", " bits  ++ ")"

