module AstView.AsmView exposing (Model, initialModel, Msg(..), subscriptions, update, view)
{-| Describe me please...
|-}

import CAsm exposing (CAsm, prettyPrint)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

-- MODEL


type alias Model =
    {
    }


initialModel : Model
initialModel = {}


-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop -> model ! []


-- VIEW


view : Model -> CAsm -> Html Msg
view model c =
    div [ class "asm-view" ]
        [ Html.pre []
            [ text <| prettyPrint c]]