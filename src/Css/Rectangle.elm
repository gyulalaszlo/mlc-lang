module Css.Rectangle exposing (..)
{-| Describe me please...
-}

import Css.Attribute exposing (Attribute, attribute)
import Css.Size exposing (Size)

type Rectangle k
    = Empty
    | All k
    | HV k k
    | Each k k k k
    | Some (Maybe k) (Maybe k) (Maybe k) (Maybe k)



sizeToS : Size -> String
sizeToS = Css.Size.toS

toAttributes : (k -> String) -> String -> Rectangle k -> List Attribute
toAttributes fn name p =
    case p of
        Empty ->
            []

        All s ->
            [ attribute name [ fn s ] ]

        HV h v ->
            [ attribute name <| List.map fn [ h, v ] ]

        Each t r b l ->
            [ attribute name <| List.map fn [ t, r, b, l ] ]

        Some t r b l ->
            List.filterMap
                (\(k,v)-> v |> Maybe.map (\vv -> attribute (name ++ "-" ++ k) [fn vv]))
                [ ("top", t), ("right", r), ("bottom", b), ("left", l) ]


concat : Rectangle k -> Rectangle k -> Rectangle k
concat a b =
    case (a,b) of
        (Empty, Empty) -> Empty
        (_, Empty) -> a

        (Some at ar ab al, Some bt br bb bl) ->
            let f a b = case (a,b) of
                    (_, Nothing) -> a
                    (_, Just _) -> b
            in Some (f at bt) (f ar br) (f ab bb) (f al bl)

        _ -> b

type alias SizeRectangle = Rectangle Size

sizeToAttributes : String -> Rectangle Size -> List Attribute
sizeToAttributes name p = toAttributes sizeToS name p



empty : Rectangle k
empty = Empty

