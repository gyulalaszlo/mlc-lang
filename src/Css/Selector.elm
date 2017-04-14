module Css.Selector exposing (..)
{-| Selector part of the Brix CSS Library
-}

import Regex


type Selector k
    = Class k
    | Children k (List (Selector k))
    | Empty
--
--
--type alias SelectorList k =
--    List (Selector k)

--type alias Selector k = k

selectorToStringList : Selector k -> List String
selectorToStringList sel =
    selectorToClassName sel
        |> List.map (\s -> "." ++ s)
--    List.map
--        (\s -> "." ++ (Regex.replace Regex.All (Regex.regex "[^a-zA-Z0-9]+") (always "") s))
--        (String.split " " <| toString sel)

selectorBitToString : k -> List String
selectorBitToString k =
    List.map
        (\s -> (Regex.replace Regex.All (Regex.regex "[^a-zA-Z0-9-_]+") (always "") s))
        (String.split " " <| toString k)


selectorToClassName : Selector k -> List String
selectorToClassName sel =

    case sel of
        Class kk -> selectorBitToString kk
--            List.map
--                (\s -> (Regex.replace Regex.All (Regex.regex "[^a-zA-Z0-9-_]+") (always "") s))
--                (String.split " " <| toString kk)

        Children k cs ->
            selectorBitToString k ++
                (List.concatMap selectorToClassName cs)

        Empty -> []


selectorToString : Selector k -> String
selectorToString selector =
    selectorToStringList selector
        |> String.join " "



map : (a -> b) -> Selector a -> Selector b
map fn a =
    case a of
        Empty -> Empty
        Children k cs -> Children  (fn k) (List.map (map fn) cs)
        Class k -> Class <| fn k
--
--andThen : (a -> Selector b) -> Selector a -> Selector b
--andThen fn a =
--    case a of
--        Child -> Child
--        Class k -> fn k
--
wrap :  Selector b -> (a -> b) -> Selector a -> Selector b
wrap w fn a =
    let children = map fn a
    in case (w,a) of
        (Empty, _) -> map fn a
        (Class ww, _) -> Children ww [children]
        (Children ww cs, _) -> Children ww <| children :: cs




--    case a of
--        Child -> Child
--        Class k -> fn a
