module Qnject.Leafs exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , css
    )
{-| Describe me please...
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Qnject.Qobject exposing (QObject)


-- MODEL

type alias Model =
    { objects : List QObject
    }


initialModel : Model
initialModel =
    { objects = []
    }


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


view : Model -> Html Msg
view model =
    div [ class "Leafs-view" ]
        [ text <| toString model ]
        
        
-- CSS

css : String
css = """
.Leafs-view {}
"""