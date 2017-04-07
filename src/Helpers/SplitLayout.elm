module Helpers.SplitLayout exposing
    ( Model
    , initialModel

    , Msg(..)

    , subscriptions

    , update, view

    , Direction(..)
    )
{-| Describe me please...
|-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)

-- MODEL

type Direction
    = Horizontal
    | Vertical


type alias Model =
    { direction: Direction
    , ratios: List Float
    }


initialModel : Model
initialModel =
    { direction = Horizontal
    , ratios = []
    }


-- MSG


type Msg
    = SetDirection Direction



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetDirection d -> {model | direction = d } ! []


-- VIEW


view : Model -> List (Html msg) -> Html msg
view model views =
    let s = styleFor views model
    in div [ class "split-layout-view" ] <|
            List.indexedMap (\i v -> singlePane v (s i) model) views

percent : number -> String
percent v = toString v ++ "%"

styleFor : List (Html msg) -> Model -> Int -> Html.Attribute msg
styleFor views model i =
    let
        w = 100.0 / (toFloat <| List.length views)
        pos = percent <| (toFloat i) * w
    in style <|
        case model.direction of
            Horizontal -> [("width", percent w), ("height", "100%"), ("left", pos)]
            Vertical -> [("height", percent w), ("width", "100%"), ("top", pos)]

singlePane : Html msg -> Html.Attribute msg -> Model -> Html msg
singlePane v styles model =
    div
        [ class "split-pane"
        , styles
        ]
        [ v ]



