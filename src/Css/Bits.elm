module Css.Bits exposing (..)

{-| Describe me please...
-}

import Color exposing (Color)
import Css.Attribute exposing (Attribute, attribute, attrsToString)
import Css.Background as Background exposing (Background(..), toAttribute)
import Css.Border exposing (Border)
import Css.Font as Font exposing (Font(..), toAttributes)
import Css.Padding
import Css.Rectangle exposing (Rectangle, SizeRectangle)
import Css.Selector as Selector exposing (Selector)
import Css.Size exposing (Size)
import Css.Theme exposing (Theme, themeToAttributes)
import Html exposing (Html)
import Html.Attributes
import Regex


type alias StyleList =
    List Attribute


type alias Font =
    Font.Font


type alias Background =
    Background.Background



--type alias Padding = Rectangle
--type alias Margin = Rectangle
-- STYLE ROW -------------------------------------------------------------------


{-| Wraps the idea that CSS is a list of nested blocks
-}
type alias StyleRow k =
    { selectors : Selector k
    , style : List Attribute
    }


row : Selector k -> List Attribute -> StyleRow k
row sel attrs =
    StyleRow sel attrs


stylesFor : k -> List Attribute -> Styles k
stylesFor k ls =
    [ StyleRow (Selector.Class k) ls ]


for : Selector k -> List (List Attribute) -> Styles k
for k ls =
    [ StyleRow k <| List.concat ls ]



-- STYLES ----------------------------------------------------------------------


type alias Styles k =
    List (StyleRow k)


empty : Styles k
empty =
    []



-- ATTRS --


classAttribute : Selector k -> Html.Attribute msg
classAttribute k =
    Selector.selectorToClassName k
        |> String.join " "
        |> Html.Attributes.class




{-|
-}
div : Selector k -> List (Html msg) -> Html msg
div k els =
    Html.div [ classAttribute k ] els


{-|
-}




class : k -> Selector.Selector k
class k =
    Selector.Class k

toHtmlClass : Selector k -> Html.Attribute msg
toHtmlClass k =
    Selector.selectorToClassName k
        |> String.join " "
        |> Html.Attributes.class


-- TO STRING  ----------------------------------------------------------------


toS : Styles k -> String
toS ss =
    let
        nonEmpty =
            List.filter (not << List.isEmpty << .style) ss

        toCssEntry { selectors, style } =
            Selector.selectorToString selectors
                ++ " {\n\t"
                ++ attrsToString style
                ++ "}"
    in
        nonEmpty
            |> List.map toCssEntry
            |> String.join "\n\n"


sizeToS : Size -> String
sizeToS =
    Css.Size.toS



--------------------------------------------------------------------------------

type alias Theme =
    Css.Theme.Theme


emptyTheme =
    Css.Theme.empty


fromThemeFor : Selector k -> Theme -> Styles k
fromThemeFor k t =
    for k <| [ themeToAttributes t ]



--------------------------------------------------------------------------------


map : Selector b -> (a -> b) -> Styles a -> Styles b
map w mapper sa =
    List.map
        (\ss -> { ss | selectors = Selector.wrap w mapper ss.selectors })
        sa



--------------------------------------------------------------------------------


px =
    Css.Size.px


em =
    Css.Size.em


pct =
    Css.Size.pct

--------------------------------------------------------------------------------

a =
    attribute


font =
    Font.toAttributes


background =
    Background.toAttribute


padding =
    Css.Padding.toAttribute


border =
    Css.Border.toAttributes

borderRadius =
    Css.Rectangle.sizeToAttributes "border-radius"

--------------------------------------------------------------------------------

rectNone =
    Css.Rectangle.Empty


all =
    Css.Rectangle.All


rectHV =
    Css.Rectangle.HV


rectEach =
    Css.Rectangle.Each

--------------------------------------------------------------------------------

top v =
    Css.Rectangle.Some (Just v) Nothing Nothing Nothing


right v =
    Css.Rectangle.Some Nothing (Just v) Nothing Nothing


bottom v =
    Css.Rectangle.Some Nothing Nothing (Just v) Nothing


left v =
    Css.Rectangle.Some Nothing Nothing Nothing (Just v)

--------------------------------------------------------------------------------

borderSolid s =
    Css.Border.WidthAndType s Css.Border.Solid

borderSolidColor s c =
    Css.Border.WidthAndTypeAndColor s Css.Border.Solid c


borderDotted s =
    Css.Border.WidthAndTypeAndColor s Css.Border.Dotted

borderDottedColor s c =
    Css.Border.WidthAndTypeAndColor s Css.Border.Dotted c


type alias SizeFn = Size -> List Attribute
type alias SizeColorFn = Size -> Color -> List Attribute

type alias SideBorderType =
    { solid: SizeFn
    , solidColor: SizeColorFn
    }

type alias Sides k =
    { top: k
    , bottom: k
    , left: k
    , right: k
    }

type alias Borders = Sides SideBorderType

borders: Borders
borders =
    { top =
        { solid = (\s -> border <| top <| borderSolid s)
        , solidColor = (\s c -> border <| top <| borderSolidColor s c)
        }
    , right =
        { solid = (\s -> border <| right <| borderSolid s)
        , solidColor = (\s c -> border <| right <| borderSolidColor s c)
        }
    , bottom =
        { solid = (\s -> border <| bottom <| borderSolid s)
        , solidColor = (\s c -> border <| bottom <| borderSolidColor s c)
        }
    , left =
        { solid = (\s -> border <| left <| borderSolid s)
        , solidColor = (\s c -> border <| left <| borderSolidColor s c)
        }
    }
