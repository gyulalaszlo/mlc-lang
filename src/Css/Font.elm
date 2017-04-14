module Css.Font exposing (..)

{-| Describe me please...
-}

import Css.Attribute exposing (Attribute, attribute)
import Css.Size exposing (Size)
import Dict exposing (Dict)
import Regex
import Set exposing (Set)


sizeToS =
    Css.Size.toS


type alias FontFamilies =
    List String


type FontAttributes
    = AttributeWeight FontWeight
    | AttributeStyle FontStyle
    | AttributeSize Size
    | AttributeFamily FontFamilies
    | AttributeLineHeight Size


type FontStyle
    = StyleNormal
    | StyleItalic


type FontWeight
    = WeightNormal
    | WeightBold
    | WeightPercent Float


type Font
    = FontAuto
    | FontFamily FontFamilies
    | FontSize Size
    | FontFamilyAndSize FontFamilies Size
    | FontFamilyAndSizeAndLineHeight FontFamilies Size Size


toAttributes : Font -> List Attribute
toAttributes f =
    case f of
        FontAuto -> []


        FontFamily n ->
            [ attribute "font-family" (fontFamilyToString n) ]

        FontSize s ->
            [ attribute "font-size" [ sizeToS s ] ]

        FontFamilyAndSize n s ->
            [ attribute "font" <| sizeToS s :: (fontFamilyToString n) ]

        FontFamilyAndSizeAndLineHeight n s h ->
            [ attribute "font" <| [sizeToS s, "/", sizeToS h] ++ (fontFamilyToString n)]


fontFamilyToString : FontFamilies -> List String
fontFamilyToString fs =
    let
        needsEscape s =
            Regex.contains (Regex.regex "[ ,;:\"]") s

        escape s =
            if needsEscape s then
                toString s
            else
                s
    in
        List.map escape fs
            |> List.intersperse ","


empty =
    FontAuto


fixedWidth =
    FontFamilyAndSize [ "Fira Code", "Monaco", "Consolas", "Courier New" ] (Css.Size.em 1)


sansSerif =
    FontFamilyAndSize [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ] (Css.Size.em 1)
