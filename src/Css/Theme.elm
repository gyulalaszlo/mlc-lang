module Css.Theme exposing (..)

{-| Describe me please...
-}

import Color exposing (Color)
import Css.Attribute exposing (Attribute)
import Css.Background
import Css.Border exposing (Border)
import Css.Font exposing (Font)
import Css.Rectangle exposing (SizeRectangle)


type alias Theme =
    { font : Font
    , background : Css.Background.Background
    , padding : SizeRectangle
    , margin : SizeRectangle
    , borderRadius : SizeRectangle
    , border : Border
    , color : Maybe Color
    }


empty : Theme
empty =
    { font = Css.Font.empty
    , background = Css.Background.empty
    , padding = Css.Rectangle.empty
    , margin = Css.Rectangle.empty
    , borderRadius = Css.Rectangle.empty
    , border = Css.Rectangle.empty
    , color = Nothing
    }


themeToAttributes : Theme -> List Attribute
themeToAttributes t =
    List.concat
        [ Css.Font.toAttributes t.font
        , Css.Background.toAttribute t.background
        , Css.Rectangle.sizeToAttributes "padding" t.padding
        , Css.Rectangle.sizeToAttributes "margin" t.margin
        , Css.Rectangle.sizeToAttributes "border-radius" t.borderRadius
        , t.color
            |> Maybe.map (\c -> Css.Attribute.attribute "color" [ Css.Attribute.colorToString c ])
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
        ]
