module Qnject.Viewers.QObjectOverview exposing
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
import Qnject.Qobject exposing (Address)


-- MODEL


type alias Model =
    { address: Address
    }


initialModel : Address -> Model
initialModel address =
    { address = address
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
    div [ class "qobject-view" ]
        [ text <| toString model
        , screenshotView model
        ]
        
        
-- CSS

css : String
css = """
.qobject-view {}
""" ++ screenshotViewCss


-- VIEW: screenshotView



{-| screenshot view
-}
screenshotView : Model -> Html Msg
screenshotView model =
    div [ class "screenshot-view" ]
        [ Html.img
            [ class "screenshot"
            , Html.Attributes.src <| "http://localhost:8000/api/qwidgets/by-address/grab/" ++ model.address
            ]
            [
            ]
        ]

{-| CSS parts for screenshotView
-}
screenshotViewCss : String
screenshotViewCss = """
.screenshot-view {  }
.screenshot-view img.screenshot { max-width:100%; }
"""


