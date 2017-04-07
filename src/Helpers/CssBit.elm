module Helpers.CssBit exposing (..)
{-| Describe me please...
-}

import Color
import Dict exposing (Dict)
import Regex


templateWith : Dict String String -> String -> String
templateWith params =
    Regex.replace Regex.All (Regex.regex <| "\\{\\{(.*)\\}\\}")
        (\{submatches, match} ->
            submatches
                |> List.head
                |> Debug.log "ASDAS"
                |> Maybe.andThen (\key -> key)
                |> Maybe.andThen (\key -> Dict.get key params)
                |> Maybe.withDefault match
                )



{-| A single dimension size (like 1px or 50% or 0.2em).
-}
type CssDimension
    = Pixel Float
    | Percent Float
    | Em Float



dim : CssDimension -> String
dim d =
    case d of
        Pixel px -> toString px ++ "px"
        Percent pc -> toString pc ++ "%"
        Em pc -> toString pc ++ "em"


negate : CssDimension -> CssDimension
negate d =
    case d of
        Pixel v -> Pixel -v
        Percent v -> Percent -v
        Em v -> Em -v



styleAttr k x vs = (k, x) :: vs
dimAttr k x vs = (k, dim x) :: vs
left = dimAttr "left"
right = dimAttr "right"
top = dimAttr "top"
bottom = dimAttr "bottom"
width = dimAttr "width"
height = dimAttr "height"

p0 = Percent 0
p100 = Percent 0

positionAbsolute = styleAttr "position" "absolute"


color c =
    let {red,green,blue,alpha} = Color.toRgb c
        bits = List.map toString [red,green,blue]
    in "rgba(" ++ String.join ", " bits ++ ", " ++ toString alpha ++ ")"

-- DEFS


--type Selector
--    = Class String Selector
--    | Id String Selector
--    | Tag String Selector
--    | Or
--    | Leaf
--
--type Css =
--    | Scope (List Selector) (List Css)
--    |


