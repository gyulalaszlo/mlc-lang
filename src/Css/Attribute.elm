module Css.Attribute exposing (..)
{-| Describe me please...
-}

import Color exposing (Color)

type alias Attribute =
    { name : String
    , values : List String
    }


attribute : String -> List String -> Attribute
attribute name val =
    Attribute name val

attrsToString : List Attribute -> String
attrsToString a =
    List.map attributeToString a
        |> String.join "\n\t"


attributeToString : Attribute -> String
attributeToString { name, values } =
    name ++ ": " ++ String.join " " values ++ ";"


colorToString : Color -> String
colorToString c =
    let
        { red, green, blue, alpha } =
            Color.toRgb c

        bits =
            List.map toString [ red, green, blue ]
    in
        "rgba(" ++ String.join ", " bits ++ ", " ++ toString alpha ++ ")"
