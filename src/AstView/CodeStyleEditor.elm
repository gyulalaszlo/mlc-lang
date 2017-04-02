module AstView.CodeStyleEditor exposing (..)
{-| Describe me please...
|-}


import CAst.CodeStyle exposing (CodeStyle, Ws, defaultCodeStyle)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

-- MODEL


type alias Model =
    { codeStyle: CodeStyle
    , selectedKey: Maybe String
    }


initialModel : Model
initialModel =
    { codeStyle = defaultCodeStyle
    , selectedKey = Nothing
    }


-- MSG


type Msg
    = Select String



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Select k -> { model | selectedKey = Just k } ! []


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ text <| toString model ]
